module Config

open FSharp.Configuration

type Settings = YamlConfig<"Config.yml">
type DesignConfig = { Spacing: float; }

let getConfig () =
    { Spacing = Settings().Designer.Spacing }
