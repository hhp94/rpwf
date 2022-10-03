"""Run nested cross validation of the scores"""
import argparse
import sys

import pandas
from sklearn.model_selection import (
    RepeatedStratifiedKFold,
    GridSearchCV,
    StratifiedKFold,
    cross_val_score,
)

# import database
# import pexp
from .. import (database, pexp)


def get_wflow_list(all_wflow: pandas.DataFrame):
    """Process the args involved in selecting wflow id"""
    if not args.rerun: # Don't run wflows that have results in db
        all_wflow = all_wflow.loc[all_wflow["result_path"].isnull(), :]
    if args.all_id:
        return all_wflow.loc[:, "wflow_id"].to_list()
    return list(set(args.wflow_id).intersection(set(all_wflow.loc[:, "wflow_id"])))

if __name__ == '__main__':
    # Parsing the arg
    parser = argparse.ArgumentParser(description="Run nested_cv of the provided wflow_ids")

    parser.add_argument("project_root", type=str, help="Root path of the project")
    parser.add_argument("-db", "--db-name", metavar="db-name", type=str,
        help="Name of the database, (e.g. 'db.SQLite')", required=True,)
    parser.add_argument("-s", "--show-wflow", action="store_true",
        help="Show list of current workflows")
    id_group = parser.add_mutually_exclusive_group()
    id_group.add_argument("-a", "--all-id", action="store_true", default=False,
        help="Run all wflows in the db")
    id_group.add_argument("-w", "--wflow-id", nargs="+", type=int,
        help="provide list of wflows to run")
    parser.add_argument("-r", "--rerun", action="store_true", default=False,
        help="Rerun wflows with completed results")
    parser.add_argument("-c", "--cores", type = int, default = -1,
        help="Number of cores for parallization")
    parser.add_argument("-icv", "--inner-n-cv", type = int, default = 5,
        help="Number of splits for the inner loop for hyper param tuning")
    parser.add_argument("-ocv", "--outer-n-cv", type = int, default = 5,
        help="Number of splits for the outer loop for cv with best hyper param")
    parser.add_argument("-ocr", "--outer-n-repeats", type = int, default = 5,
        help="Number of repeats of splits for the outer loop for cv with best hyper param")
    # parser.add_argument("-ls", "--learner-silent", type = bool, default = True,
    #     help="Silent output of learner")
    # parser.add_argument("-lv", "--learner-verbose", type = int, default = 0,
    #     help="Verbosity state of learner")

    args = parser.parse_args()

    # Setup the base objects
    db_obj = database.Base(args.project_root, args.db_name)
    wflow_df = db_obj.all_wflow()

    if args.show_wflow:
        print(wflow_df)
        sys.exit()

    wflow_list = get_wflow_list(wflow_df)
    if not wflow_list:
        print("All wflows have results, exiting...")
        sys.exit()

    print(f"running {wflow_list}")
    # print(vars(args))

    # Run the experiment
    for wflow_id in wflow_list:
        print(f"running wflow {wflow_id}")
        wflow_obj = pexp.PexpWflow(db_obj, id)
        n_cores = args.cores

        # Generate the parameters
        p_grid = pexp.PexpGrid(db_obj, wflow_obj).get_grid()

        df_obj = pexp.PexpDf(db_obj, wflow_obj)
        X, y = df_obj.get_df_X(), df_obj.get_df_y()

        model_type_obj = pexp.PexpModelParam(db_obj, wflow_obj)
        base_learner = pexp.PexpLearner(wflow_obj, model_type_obj).base_learner
        score = pexp.PexpCost(db_obj, wflow_obj).get_cost()

        # Nested resampling
        inner_cv = StratifiedKFold(
            n_splits=args.inner_n_cv, shuffle=True, random_state=wflow_obj.random_state
        )
        outer_cv = RepeatedStratifiedKFold(
            n_splits=args.outer_n_cv, n_repeats=args.outer_n_repeats,
            random_state=wflow_obj.random_state
        )

        param_tuner = GridSearchCV(
            estimator=base_learner,
            param_grid=p_grid,
            cv=inner_cv,
            n_jobs=n_cores,
            scoring=score,
        )
        nested_score = cross_val_score(param_tuner, X=X, y=y, cv=outer_cv)

        # Export the results
        exporter = pexp.PexpExport(db_obj, wflow_obj)
        nested_score_df = pandas.DataFrame(nested_score, columns=[score])
        exporter.export_cv(nested_score_df, "nested_cv")
        exporter.export_db()
