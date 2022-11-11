"""Paths and database connections"""
from __future__ import annotations
import os
import pathlib
from typing import List

import sqlalchemy
import pandas


def singleton(class_):
    """Create a Borg singleton implementation"""
    instances = {}

    def getinstance(*args, **kwargs):
        if class_ not in instances:
            instances[class_] = class_(*args, **kwargs)
        return instances[class_]
    return getinstance


@singleton
class Base:
    """Initiate the meta_dat, the engine object, and paths, for database connection.
    This would be a singleton class."""

    def __init__(self, proj_root_path: str, db_name: str) -> None:
        """Base is a singleton. We assign the paths and database object to this class"""
        # Setting up the paths
        self.proj_root_path: pathlib.Path = pathlib.Path(proj_root_path)
        assert "rpwfDb" in os.listdir(
            self.proj_root_path
        ), "rpwfDb folder not found in provided root path"

        self.db_path: pathlib.PurePosixPath = pathlib.PurePosixPath(
            self.proj_root_path.joinpath("rpwfDb", db_name)
        )
        print(f"db is at {self.db_path}")

        assert (
            os.path.exists(str(self.db_path)) is True
        ), f"rpwfDb folder found, but {db_name} db not found"

        self.result_path: pathlib.Path = self.proj_root_path.joinpath(
            "rpwfDb", f"{db_name}_results"
        )
        if self.result_path.exists() is False:
            print(f"Creating the {db_name}_results folder")
            self.result_path.mkdir()  # create results folder

        # Database objects future enables 2.0 style syntax
        print("Connecting to " + f"sqlite:///{str(self.db_path)}")
        self.engine: sqlalchemy.future.engine.Engine = sqlalchemy.create_engine(
            f"sqlite:///{str(self.db_path)}", future=True
        )

        # Binding allows for dict like syntax
        self.meta_dat: sqlalchemy.MetaData = sqlalchemy.MetaData()
        self.meta_dat.reflect(bind=self.engine)

    def all_wflow(self) -> None:
        """Show the wflow list"""
        w_tbl = self.meta_dat.tables["wflow_tbl"]
        wr_tbl = self.meta_dat.tables["wflow_result_tbl"]

        query: sqlalchemy.sql.selectable.Select = (
            sqlalchemy.select(w_tbl, wr_tbl.c.result_path)
            .select_from(w_tbl)
            .join(wr_tbl, isouter=True)
        )  # Left join

        results: List[tuple] = []
        with self.engine.connect() as conn:
            for row in conn.execute(query):
                results.append(row)

        return pandas.DataFrame(results).loc[
            :, ["wflow_id", "model_tag", "recipe_tag", "py_base_learner_args", "result_path"]
        ]

    def __repr__(self) -> str:
        return f"{self.engine}"
