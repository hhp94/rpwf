"""Paths and database connections"""
from __future__ import annotations
import pathlib
from typing import List, Any

import importlib
import pandas
import sqlalchemy
import yaml

def singleton(class_):
    """Create a Borg singleton implementation"""
    instances = {}

    def getinstance(*args, **kwargs):
        if class_ not in instances:
            instances[class_] = class_(*args, **kwargs)
        return instances[class_]
    return getinstance


@singleton
class Board:
    def __init__(self, yml_path: str):
        with open(pathlib.Path(yml_path), "r") as board_yml:
            try:
                self.board_meta = yaml.safe_load(board_yml)
            except yaml.YAMLError as exc:
                raise print(exc)
        self.board_type = self.board_meta.pop('board')
        self.set_board()
    
    def set_board(self) -> None:
        if(self.board_type == "pins_board_folder"):
            self.board_constructor = getattr(importlib.import_module("pins"),"board_folder")
            self.board = self.board_constructor(**self.board_meta)
            return None
        raise NameError(f"{self.board_type} is currently not implemented")

    def __repr__(self) -> str:
        return str(self.board)


@singleton
class Base:
    """Initiate the meta_dat, the engine object, and paths, for database connection.
    This would be a singleton class."""

    def __init__(self, db_path: str, **kwargs: Any) -> None:
        """Base is a singleton. We assign the database object to this class"""
        # Setting up the paths
        assert pathlib.Path(db_path).exists() is True, "db not found"
        self.db_path: pathlib.PurePosixPath = pathlib.PurePosixPath(db_path)

        # Database objects future enables 2.0 style syntax
        print("Connecting to " + f"sqlite:///{str(self.db_path)}")
        self.engine: sqlalchemy.future.engine.Engine = sqlalchemy.create_engine(
            f"sqlite:///{str(self.db_path)}", future=True, **kwargs
        )

        # Binding allows for dict like syntax
        self.meta_dat: sqlalchemy.MetaData = sqlalchemy.MetaData()
        self.meta_dat.reflect(bind=self.engine)

    def all_wflow(self) -> None:
        """Show the wflow list"""
        w_tbl = self.meta_dat.tables["wflow_tbl"]
        wr_tbl = self.meta_dat.tables["wflow_result_tbl"]

        query: sqlalchemy.sql.selectable.Select = (
            sqlalchemy.select(w_tbl, wr_tbl.c.result_pin_name, wr_tbl.c.model_pin_name)
            .select_from(w_tbl)
            .join(wr_tbl, isouter=True)
        )  # Left join

        results: List[tuple] = []
        with self.engine.connect() as conn:
            for row in conn.execute(query):
                results.append(row)
        assert len(results) > 0, "No wflow found. Perhaps run `rpwf_export_db()` in R?"

        return pandas.DataFrame(results).loc[
            :, ["wflow_id", "model_tag", "recipe_tag", "result_pin_name", "model_pin_name"]
        ]

    def __repr__(self) -> str:
        return f"{self.engine}"
