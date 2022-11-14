"""Main module that contains classes to interact with the database"""
from __future__ import annotations
import importlib
import json
import pathlib
from typing import Any, Dict, Union

import joblib
import numpy
import pdcast
import pins
import pyarrow.parquet
import pandas
import sqlalchemy

from .database import Base


class _RpwfSQL:
    """This class accepts the Base singleton and contains 2 common methods,
    `_exec_query()` to set and exec SQL query. `_import_parquet()` to read in
    parquets"""

    def __init__(self, base: Base) -> None:
        """Not meant to be run by users"""
        self.base = base  # Inject dependency of the Base class
        self.query: sqlalchemy.sql.selectable.Select = None
        self.query_results: dict = None
        self.parquet = None

    def _exec_query(self) -> None:
        """Execute the sql query, results stored as list of dict with column
        names as keys and single values of the row as values. Atrribute is
        self.query_results"""
        try:
            with self.base.engine.connect() as conn:
                self.query_results: Dict[str, Any] = [
                    row._asdict() for row in conn.execute(self.query)
                ][0]
        except Exception as exc:
            raise ValueError("Query matched no id") from exc

    def _import_parquet(self, df_pin_name: str) -> None:
        """Create attribute `self.parquet` file for importing"""
        # If no grid is used, then `parquet_path` would be NA
        if (parquet_path := self.query_results.get(df_pin_name, None)) is not None:
            self.parquet = pyarrow.parquet.read_table(
                self.base.db_path.joinpath(parquet_path)
            )


class Wflow(_RpwfSQL):
    """Main object that represents a workflow, should be a singleton"""

    def __init__(self, base: Base, wflow_id: int) -> None:
        """In addition to super(), have self.wflow_id and self.random_state"""
        super().__init__(base)
        self.query = sqlalchemy.select(self.base.meta_dat.tables["wflow_tbl"]).where(
            self.base.meta_dat.tables["wflow_tbl"].c.wflow_id == wflow_id
        )  # Returns the parameters of the wflow
        self._exec_query()
        self.wflow_id = wflow_id
        self.args_json = self._get_par("py_base_learner_args")
        self.random_state = self._get_par("random_state")

    def _get_par(self, param: str) -> int:
        """Get the cost_id, model_type_id, etc. that's associated with the wflow_id"""
        return self.query_results[param]

    def _get_base_learner_args(self) -> dict:
        """Get the arguments for the base learners"""
        if self.args_json is None:
            return None
        return json.loads(self.args_json)

    def __repr__(self) -> str:
        return str(self.query_results)


class RGrid(_RpwfSQL):
    """Import the grid generated in R"""

    def __init__(self, base: Base, wflow: Wflow) -> None:
        """Get the grid associated with the wflow"""
        super().__init__(base)
        self.grid_id: int = wflow._get_par("grid_id")
        self.query = sqlalchemy.select(
            self.base.meta_dat.tables["r_grid_tbl"].c.grid_pin_name
        ).where(self.base.meta_dat.tables["r_grid_tbl"].c.grid_id == self.grid_id)
        self._exec_query()
        self._import_parquet("grid_pin_name")

    def val_to_list(self, d: Dict):
        for v in d:
            d[v] = [d[v]]
        return d

    def get_grid(self) -> Dict[str, Any]:
        """Read in the parquet file and return as dict"""
        if self.parquet is None:
            return None
        # convert the parquet file to list of rows
        grid = self.parquet.to_pylist()
        # wrap the value of each row in a list
        l = len(grid)
        wrapped_grid = [None] * l
        for i in range(l):
            wrapped_grid[i]= self.val_to_list(grid[i])
        return wrapped_grid


class TrainDf(_RpwfSQL):
    """Import the train data frame in parquet"""

    def __init__(self, base: Base, wflow: Wflow, downcast: bool = True) -> None:
        """Get the train data associated with the wflow"""
        super().__init__(base)
        self.df_id: int = wflow._get_par("df_id")
        self.query = sqlalchemy.select(self.base.meta_dat.tables["df_tbl"]).where(
            self.base.meta_dat.tables["df_tbl"].c.df_id == self.df_id
        )
        self._exec_query()
        self._index: Union[None, str] = self.query_results["idx_col"]
        self._target: Union[None, str] = self.query_results["target"]
        self._import_parquet("df_pin_name")  # Import the parquet file
        self._get_df(downcast)  # Convert to pandas DataFrame, can perform downcast

    def _get_df(self, downcast) -> None:
        """Convert to pandas DataFrame"""
        if downcast:
            # Downcast the numeric columns to save memory and generate index
            min_schema = pdcast.infer_schema(self.parquet.to_pandas())
            try: # Exclude the target from downcasting, a test df's target is None
                del min_schema[self._target]
            except KeyError: # if target column not provided then cannot try to delete
                pass
            self.df = pdcast.coerce_df(self.parquet.to_pandas(), min_schema)
        else:
            self.df = self.parquet.to_pandas()

        if self._index:
            self.df.set_index(self._index, drop=True, inplace=True)
        assert self._index not in self.df.columns, f"{self._index} is still in the data"

    def get_df_X(self, to_ndarray = False) -> Union[numpy.ndarray, pandas.DataFrame]:
        """Return the predictor DataFrame"""
        df_X = self.df.loc[:, json.loads(self.query_results["predictors"])]
        if to_ndarray:
          return df_X.to_numpy()
        return df_X

    def get_df_y(self, to_ndarray = False) -> Union[None, numpy.ndarray, pandas.DataFrame]:
        """Return the response Series"""
        if self._target is None:
            print("no target column provided")
            return None
        df_y = self.df.loc[:, self.df.columns == self._target].to_numpy()
        if to_ndarray:
          return df_y.to_numpy()
        return df_y.to_numpy()

# class Cost(_RpwfSQL):
#     """Get the cost metrics (RMSE, neg_log_loss etc.)"""

#     def __init__(self, base: Base, wflow: Wflow) -> None:
#         """Get the cost metric associated with the wflow"""
#         super().__init__(base)
#         self.cost_id: int = wflow._get_par("cost_id")
#         self.query = sqlalchemy.select(
#             self.base.meta_dat.tables["cost_tbl"].c.cost_name
#         ).where(self.base.meta_dat.tables["cost_tbl"].c.cost_id == self.cost_id)
#         self._exec_query()

#     def get_cost(self) -> str:
#         """Cost metric for optimization e.g. log_loss"""
#         return self.query_results["cost_name"]


class Model(_RpwfSQL):
    """Query the database for the base_learner definitions"""

    def __init__(self, base: Base, wflow: Wflow) -> None:
        """Get the model parameters"""
        super().__init__(base)
        self.model_type_id: int = wflow._get_par("model_type_id")
        self.query = sqlalchemy.select(
            self.base.meta_dat.tables["model_type_tbl"]
        ).where(
            self.base.meta_dat.tables["model_type_tbl"].c.model_type_id
            == self.model_type_id
        )
        self._exec_query()

    def _get_py_module(self) -> str:
        """Get the py module e.g. xgboost lightgbm"""
        return self.query_results["py_module"]

    def _get_base_learner(self) -> str:
        """Get the base learner such as XGBClassifier"""
        return self.query_results["py_base_learner"]


class BaseLearner:
    """Create the base leaner as defined by the database"""

    def __init__(self, wflow: Wflow, model_param: Model) -> None:
        """Build the base learner from the Model and Wflow objects"""
        learner_module = getattr(
            importlib.import_module(f"{model_param._get_py_module()}"),
            f"{model_param._get_base_learner()}"
        )
        print(f"Running {learner_module}")
        if wflow.args_json is None:
            self._base_learner = learner_module(random_state=wflow.random_state)
        else:
            self._base_learner = learner_module(
                random_state=wflow.random_state, **wflow._get_base_learner_args()
            )  # Pass additional arguments to the base learners

    @property
    def base_learner(self):
        """Return the baselearner object"""
        return self._base_learner

    def __repr__(self) -> str:
        return f"{self._base_learner}"


class Export(_RpwfSQL):
    """Export the cv results, model files of the workflow into the db"""

    def __init__(self, base: Base, wflow: Wflow) -> None:
        """Export pathing variables are handled upon calling export functions"""
        super().__init__(base)
        self.wflow_id: int = wflow.wflow_id  # Set the wflow_id
        self.wflow_hash: int = hash(wflow)
        self.res_tbl = self.base.meta_dat.tables["wflow_result_tbl"]
        self._wflow_results_query()
        self.desc: str = None
        self.csv_path: str = None
        self.model_path: str = None
        self.export_query = None

    def _wflow_results_query(self) -> None:
        query = sqlalchemy.select(self.res_tbl).where(
            self.res_tbl.c.wflow_id == self.wflow_id
            )
        with self.base.engine.connect() as conn:
            try:
                self.query_results: Dict[str, Any] = [
                    row._asdict()
                    for row in conn.execute(query)
                ][0]
            except IndexError:
                print("No results for this wflow in the db")

    def _gen_rel_path(self, path: pathlib.Path) -> str:
        """Generate relative paths to store in the database"""
        return str(pathlib.PurePosixPath(path.relative_to(self.base.db_path)))

    def export_cv(self, results: pandas.DataFrame, desc: str) -> None:
        """Export the cross validation results, expects pd.DataFrame results"""
        self.desc = desc  # descriptions of the result (sampling schemes)
        self.csv_path = self.base.result_path.joinpath(
            f"wflow_{self.wflow_id}_{self.desc}_{self.wflow_hash}_results.csv"
        )
        results.to_csv(self.csv_path, index=False)  # res_tblite the csv
        self.csv_path = self._gen_rel_path(self.csv_path)  # To rel path after export

    def _set_export_query(self) -> None:
        """Set the proper export query for adding rows to result table"""
        entries = {
            "wflow_id": self.wflow_id,
            "description": self.desc,
            "result_path": self.csv_path,
            "model_path": self.model_path,
        }
        if self.query_results:
            # Change `insert` to `update` if the wflow_id matches
            print("Found in db, overwriting")
            self.export_query = (
                sqlalchemy.update(self.res_tbl)
                .where(self.res_tbl.c.wflow_id == self.wflow_id)
                .values(**entries)
            )
            return None
        print("Writing to db")
        self.export_query = sqlalchemy.insert(self.res_tbl).values(**entries)
        return None

    def export_db(self) -> None:
        """After exporting the cv results, we update the database"""
        # Assert that the exported files exists
        try:
            assert self.base.db_path.joinpath(
                self.csv_path
            ).exists(), """Run export_cv() first"""
            if self.model_path:
                assert self.base.db_path.joinpath(self.model_path).exists()
        except TypeError as new_entry_error:
            raise TypeError("Run export_cv() first") from new_entry_error
        except AssertionError as file_not_exists:
            raise AssertionError(
                """Entry in db found but file not found. Rerun export_cv().
                Or also export_model()"""
            ) from file_not_exists

        self._set_export_query()
        with self.base.engine.connect() as conn:
            print("Exporting to db")
            result = conn.execute(self.export_query)
            conn.commit()
            self._wflow_results_query()  # After write new row, update query res
            return result

    def export_model(self, model: Any) -> None:
        """Export the fitted model if needed, run before export_db()"""
        self.model_path = self.base.result_path.joinpath(
            f"wflow_{self.wflow_id}_{self.desc}_{self.wflow_hash}_model.joblib"
        )
        joblib.dump(model, self.model_path)
        self.model_path = self._gen_rel_path(self.model_path)

    def __repr__(self) -> str:
        return str(self.query_results)


# class CleanResults:
#     """Clean the results generated by cross valdiation before exporting"""

#     def __init__(self, wflow: Wflow) -> None:
#         self.cost = wflow._get_par("")
