module Config

open FSharp.Configuration

type Settings = AppSettings<"app.config">
type DesignConfig = { Spacing: float; }
