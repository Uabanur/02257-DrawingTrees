module Config

open FSharp.Configuration

type Settings = YamlConfig<"Config.yml">
type DesignConfig = { HorizontalSpacing: float; }
type RenderMode =  | Plain | Alternative
type RenderConfig = { Mode: RenderMode; VerticalSpacing: float; }

let getRenderMode (mode: string) =
    match mode with
    | "Plain" -> Plain
    | "Alternative" -> Alternative
    | _ -> failwith "Invalid render mode"

let getConfig () =
    {
        HorizontalSpacing = Settings().Designer.HorizontalSpacing;
    },
    {
        Mode = getRenderMode <| Settings().Renderer.Mode;
        VerticalSpacing = Settings().Designer.VerticalSpacing
    }
