"""Run nested cross validation of the scores"""
from __future__ import annotations
import argparse
from pathlib import Path
import sys

import pandas
from numpy import ravel
from sklearn.model_selection import (
    RepeatedStratifiedKFold,
    RepeatedKFold,
    GridSearchCV,
    cross_val_score
)

from .. import database, rpwf


def get_wflow_list(all_wflow: pandas.DataFrame):
    """If the workflow has results in the db or not"""
    if not args.force:  # Don't run wflows that have results in db
        all_wflow = all_wflow.loc[all_wflow["result_pin_name"].isnull(), :]
    if args.all_id:
        return all_wflow.loc[:, "wflow_id"].to_list()
    return list(set(args.wflow_id).intersection(set(all_wflow.loc[:, "wflow_id"])))


if __name__ == "__main__":
    # Parsing the arg
    parser = argparse.ArgumentParser(
        description="run nested_cv of the provided wflow_ids"
    )

    parser.add_argument(
        "db_path",
        type=str,
        help="path to the database"
    )
    parser.add_argument(
        "-b",
        "--board",
        metavar="board",
        required=True,
        type=str,
        help="path to the yaml file of the board"
    )

    id_group = parser.add_mutually_exclusive_group(required=True)
    id_group.add_argument(
        "-s", "--show-wflow", action="store_true", help="show list of current workflows"
    )
    id_group.add_argument(
        "-a",
        "--all-id",
        action="store_true",
        default=False,
        help="run all wflows in the db"
    )
    id_group.add_argument(
        "-w",
        "--wflow-id",
        metavar="wflow-id",
        nargs="+",
        type=int,
        help="input list of wflows to run"
    )

    parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        default=False,
        help="force runnning of wflows"
    )
    parser.add_argument(
        "-c",
        "--cores",
        metavar="cores",
        type=int,
        default=1,
        help="number of cores for parallization"
    )
    parser.add_argument(
        "-ns",
        "--n-splits",
        metavar="n-splits",
        type=int,
        default=5,
        help="number of splits for the cross-validation"
    )
    parser.add_argument(
        "-nr",
        "--n-repeats",
        metavar="n-repeats",
        type=int,
        default=1,
        help="number of repeats for the cross-validation"
    )
    # parser.add_argument(
    #     "-e",
    #     "--export",
    #     action="store_true",
    #     help="export results to database or not"
    # )
    parser.add_argument(
        "-j",
        "--joblib-model",
        action="store_true",
        help="export model as a joblib object"
    )
    args = parser.parse_args()

    # Setup the base objects
    db_obj = database.Base(Path(args.db_path).as_posix())
    board_obj = database.Board(Path(args.board).as_posix())
    wflow_df = db_obj.all_wflow()

    # Show the wflows and exit
    if args.show_wflow:
        print(wflow_df)
        sys.exit()

    wflow_list = get_wflow_list(wflow_df)
    if not wflow_list:
        print("Either invalid wflow or all requested wflow already have results")
        sys.exit()

    print(f"running {wflow_list}")
    # print(vars(args)) # for debug the args

    # Run the experiment
    for wflow_id in wflow_list:
        print(f"running wflow {wflow_id}")
        wflow_obj = rpwf.Wflow(db_obj, board_obj, wflow_id)
        n_cores = args.cores

        # Generate the parameters
        p_grid = rpwf.RGrid(db_obj, board_obj, wflow_obj).get_grid()

        df_obj = rpwf.TrainDf(db_obj, board_obj, wflow_obj)
        X, y = df_obj.get_df_X(True), df_obj.get_df_y(True)

        if y is None:
            print("No target provided, exiting...")
            sys.exit()
        
        y = ravel(y)
        
        model_type_obj = rpwf.Model(db_obj, board_obj, wflow_obj)
        base_learner = rpwf.BaseLearner(wflow_obj, model_type_obj).base_learner
        score = wflow_obj._get_par("costs")

        if (model_mode := model_type_obj._get_model_mode()) == 'regression':
            vfold_cv = RepeatedKFold
        elif model_mode == 'classification':
            vfold_cv = RepeatedStratifiedKFold
        else:
            raise ValueError("Either `regression` or `classification` is expected")

        cv = vfold_cv(
            n_splits=args.n_splits,
            n_repeats=args.n_repeats,
            random_state=wflow_obj.random_state,
        )

        if p_grid is None:
            print("No tune grid specified, running with default params")
            cv_results = cross_val_score(
                base_learner, X=X, y=y, cv=cv, n_jobs=n_cores, scoring=score
            )
        else:
            print("Performing nested-cv using provided Rgrid")
            param_tuner = GridSearchCV(
                estimator=base_learner,
                param_grid=p_grid,
                cv=cv,
                n_jobs=n_cores,
                scoring=score
            )
            param_tuner.fit(X=X, y=y)
            tuning_results = pandas.DataFrame(param_tuner.cv_results_)
            cv_results = tuning_results.loc[tuning_results['rank_test_score'] == 1]

        # if args.export:
        # Export the results
        exporter = rpwf.Export(db_obj, board_obj, "cv", wflow_obj)
        exporter.export_cv(pandas.DataFrame(cv_results))
        if args.joblib_model and param_tuner:
            exporter.export_model(param_tuner.best_estimator_)
        exporter.export_db()
