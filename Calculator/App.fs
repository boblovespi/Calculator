module Calculator.App

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

type State =
    { expr: string
      result: string
      history: (string * string) list }

let init () =
    { expr = ""; result = ""; history = [] }

type Msg =
    | UpdateExpr of string
    | AddToHistory

let update msg (state: State) =
    match msg with
    | UpdateExpr expr ->
        match expr |> Parser.lex |> Parser.parse with
        | None ->
            { expr = expr
              result = "Parse Error"
              history = state.history }
        | Some value ->
            try
                let result = MathEngine.compute value in

                { expr = expr
                  result = $"{result}"
                  history = state.history }
            with _ ->
                { expr = expr
                  result = "Computation Error"
                  history = state.history }
    | AddToHistory ->
        { expr = ""
          result = ""
          history = (state.expr, state.result) :: state.history }

let view (state: State) dispatch : Types.IView =
    Border.create
        [ Border.margin 10
          Border.child (
              DockPanel.create
                  [ // DockPanel.spacing 10
                    DockPanel.children
                        [ TextBox.create
                              [ TextBox.text state.expr
                                TextBox.dock Dock.Top
                                TextBox.onTextChanged (UpdateExpr >> dispatch)
                                //TextBox.keyBindings [ KeyGesture(Key.Enter).create ]
                                TextBox.onKeyUp (fun k ->
                                    match k.PhysicalKey with
                                    | PhysicalKey.Enter ->
                                        do
                                            dispatch AddToHistory
                                            k.Handled <- true
                                    | _ -> ()) ]
                          SelectableTextBlock.create
                              [ SelectableTextBlock.margin 5
                                SelectableTextBlock.dock Dock.Top
                                SelectableTextBlock.text state.result ]
                          ScrollViewer.create
                              [ ScrollViewer.content (
                                    StackPanel.create
                                        [ StackPanel.spacing 10
                                          StackPanel.children (
                                              state.history
                                              |> List.map (fun (expr, res) ->
                                                  Button.create
                                                      [ Button.content $"{expr} = {res}"
                                                        Button.onClick (
                                                            (fun _ -> expr |> UpdateExpr |> dispatch),
                                                            SubPatchOptions.OnChangeOf expr
                                                        ) ])
                                          ) ]
                                ) ] ] ]
          ) ]

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Calculator"
        base.Width <- 400
        base.Height <- 1000

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
