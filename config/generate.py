from __future__ import annotations

import abc
import dataclasses
from pathlib import Path
from typing import Any, NamedTuple, Union

import jinja2
import yaml

DATA_FILE = Path("./data.yaml")

def main():
    data = ConfigData.load()
    data.render("ConfigGen.jinja", "../src/Ormolu/Config/Gen.hs")
    data.render("fourmolu_yaml.jinja", "../fourmolu.yaml")

HaskellValue = Union[str, int, bool, list]

class ConfigData(NamedTuple):
    options: list[ConfigOption]
    fieldTypes: list[ConfigFieldType]

    @classmethod
    def load(cls) -> ConfigData:
        data = yaml.safe_load(DATA_FILE.read_text())
        return cls.parse(data)

    @classmethod
    def parse(cls, data: dict) -> ConfigData:
        options = [
            ConfigOption.parse(option)
            for option in data["options"]
        ]
        fieldTypes = [
            ConfigFieldType.parse(fieldType)
            for fieldType in data["fieldTypes"]
        ]
        return cls(options=options, fieldTypes=fieldTypes)

    @property
    def fieldTypesMap(self) -> dict[str, ConfigFieldType]:
        return {fieldType.name: fieldType for fieldType in self.fieldTypes}

    def render(self, template_file: str, output_file: str):
        env = jinja2.Environment(loader=jinja2.FileSystemLoader("."))

        variables = {
            **self._asdict(),
            "hs2yaml": self.haskell_to_yaml,
            "get_cli_help": self.render_cli_help,
            "get_cli_placeholder": self.render_cli_placeholder,
        }

        template = env.get_template(template_file)
        output = template.render(**variables)
        Path(output_file).write_text(output)

    def haskell_to_yaml(self, option: ConfigOption, v: HaskellValue) -> str:
        fieldType = self.fieldTypesMap.get(option.type)
        if fieldType:
            return fieldType.render_haskell_value(v)

        if isinstance(v, bool):
            return "true" if v else "false"
        elif isinstance(v, int):
            return str(v)
        elif isinstance(v, str):
            return v
        elif isinstance(v, list):
            return "[" + ", ".join(self.haskell_to_yaml(x) for x in v) + "]"
        else:
            raise ValueError(f"Could not render value: {v}")

    def render_cli_help(self, option: ConfigOption) -> str:
        fieldType = self.fieldTypesMap.get(option.type)

        help_text = option.cliHelp
        if not help_text:
            help_text = option.description

        choices_text = ""
        if isinstance(fieldType, ConfigFieldTypeEnum):
            choices_text = f" (choices: {fieldType.options_display})"

        default_display = option.cliDefault
        if not default_display:
            default_display = self.haskell_to_yaml(option, option.default)
        default_text = f" (default: {default_display})"

        return help_text + choices_text + default_text

    def render_cli_placeholder(self, option: ConfigOption) -> str:
        if option.cliPlaceholder:
            return option.cliPlaceholder
        elif option.type == "Bool":
            return "BOOL"
        elif option.type == "Int":
            return "INT"
        else:
            return "OPTION"

@dataclasses.dataclass
class ConfigOption:
    name: str
    fieldName: str | None
    description: str
    type: str
    default: HaskellValue
    ormolu: HaskellValue
    cliHelp: str | None
    cliDefault: str | None
    cliPlaceholder: str | None

    @classmethod
    def parse(cls, data: dict) -> ConfigOption:
        name = data["name"]
        fieldName = data.get("fieldName")
        description = data["description"]
        type = data["type"]
        default = data["default"]
        ormolu = data["ormolu"]

        cli = data.get("cli", {})
        cliHelp = cli.get("help")
        cliDefault = cli.get("default")
        cliPlaceholder = cli.get("placeholder")

        return cls(
            name=name,
            fieldName=fieldName,
            description=description,
            type=type,
            default=default,
            ormolu=ormolu,
            cliHelp=cliHelp,
            cliDefault=cliDefault,
            cliPlaceholder=cliPlaceholder,
        )

@dataclasses.dataclass
class ConfigFieldType(abc.ABC):
    name: str

    @classmethod
    def parse(cls, data: dict) -> ConfigFieldType:
        if "enum" in data:
            return ConfigFieldTypeEnum.parse(data)
        elif "adt" in data:
            return ConfigFieldTypeADT.parse(data)
        else:
            raise ValueError(f"Could not parse field type: {data}")

    @abc.abstractmethod
    def render_haskell_value(self, v: HaskellValue) -> str:
        pass

    @property
    def enum(self):
        return self if isinstance(self, ConfigFieldTypeEnum) else None

    @property
    def adt(self):
        return self if isinstance(self, ConfigFieldTypeADT) else None

@dataclasses.dataclass
class ConfigFieldTypeEnum(ConfigFieldType):
    # maps Haskell constructor to rendered string
    options: dict[str, str]

    @classmethod
    def parse(cls, data: dict) -> ConfigFieldTypeEnum:
        name = data["name"]

        options = {}
        for option in data["enum"]:
            assert len(option) == 1
            constructor, value = list(option.items())[0]
            options[constructor] = value

        return cls(name=name, options=options)

    def render_haskell_value(self, v: HaskellValue) -> str:
        return self.options[v]

    @property
    def options_display(self) -> str:
        return render_list([f'\\"{opt}\\"' for opt in self.options.values()])

@dataclasses.dataclass
class ConfigFieldTypeADT(ConfigFieldType):
    constructors: list[str]
    render: dict[HaskellValue, str]
    parseJSON: str
    parsePrinterOptType: str

    @classmethod
    def parse(cls, data: dict) -> ConfigFieldTypeADT:
        name = data["name"]
        adt = data["adt"]
        constructors = adt["constructors"]
        render = adt["render"]
        parseJSON = adt["parseJSON"].strip()
        parsePrinterOptType = adt["parsePrinterOptType"].strip()
        return ConfigFieldTypeADT(
            name=name,
            constructors=constructors,
            render=render,
            parseJSON=parseJSON,
            parsePrinterOptType=parsePrinterOptType,
        )

    def render_haskell_value(self, v: HaskellValue) -> str:
        return self.render[v]

def render_list(vs, sep="or"):
    if len(vs) == 0:
        return ""
    elif len(vs) == 1:
        return vs[0]
    elif len(vs) == 2:
        return f"{vs[0]} {sep} {vs[1]}"
    else:
        init, last = vs[:-1], vs[-1]
        return ", ".join([*init, f"{sep} {last}"])

if __name__ == "__main__":
    main()
