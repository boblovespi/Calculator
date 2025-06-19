module Calculator.App

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

type State = { expr: string; result: string }
let init () = { expr = ""; result = "" }
type Msg = UpdateExpr of string

let update (UpdateExpr expr) (_state: State) =
    let parsed = expr |> Parser.lex |> Parser.parse in

    match parsed with
    | None -> { expr = expr; result = "Parse Error" }
    | Some value ->
        try
            let result = MathEngine.compute value in { expr = expr; result = $"{result}" }
        with _ ->
            { expr = expr
              result = "Computation Error" }

let view (state: State) dispatch : Types.IView =
    Border.create
        [ Border.margin 10
          Border.child (
              StackPanel.create
                  [ StackPanel.children
                        [ TextBox.create [ TextBox.text state.expr; TextBox.onTextChanged (UpdateExpr >> dispatch) ]
                          TextBlock.create [ TextBlock.text state.result ] ] ]
          ) ]

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Calculator"
        base.Width <- 400
        base.Height <- 100

        let subscriptions _state =
            let onClosedSub _dispatch =
                this.Closed.Subscribe(fun _ -> printfn "The window has been closed.") in

            [ [ nameof onClosedSub ], onClosedSub ]

        Elmish.Program.mkSimple init update view
        |> Elmish.Program.withHost this
        |> Elmish.Program.withSubscription subscriptions
        |> Elmish.Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()
