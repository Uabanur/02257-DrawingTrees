module Config

open FSharp.Configuration

type Settings = YamlConfig<"Config.yml">
type DesignConfig = { Spacing: float; }
type RenderMode =  | Plain | Alternative
type RenderConfig = { Mode: RenderMode; }

let getRenderMode (mode: string) =
    match mode with
    | "Plain" -> Plain
    | "Alternative" -> Alternative
    | _ -> failwith "Invalid render mode"

let getConfig () =
    { Spacing = Settings().Designer.Spacing }, { Mode = getRenderMode <| Settings().Renderer.Mode }
