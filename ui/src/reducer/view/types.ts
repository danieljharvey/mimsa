import { EditorState } from '../editor/types'
import * as NE from 'fp-ts/NonEmptyArray'

export type EditScreen = {
    type: 'edit'
    bindingName: string
    editor: EditorState
}

export type NewExpressionScreen = {
    type: 'new-expression'
    editor: EditorState
}

export type NewTestScreen = {
    type: 'new-test'
    editor: EditorState
}

export type ScratchScreen = {
    type: 'scratch'
    editor: EditorState
}

export type TypeSearchScreen = {
    type: 'typeSearch'
}

export type NewTypeScreen = {
    type: 'new-type'
    editor: EditorState
}

export type Screen =
    | ScratchScreen
    | EditScreen
    | NewExpressionScreen
    | NewTestScreen
    | TypeSearchScreen
    | NewTypeScreen

export type ViewState = {
    stack: NE.NonEmptyArray<Screen>
}

export type ViewAction =
    | { type: 'SetScreen'; screen: Screen }
    | { type: 'PushScreen'; screen: Screen }
    | { type: 'ReplaceScreen'; screen: Screen }
    | { type: 'PopScreen' }

export type ViewEvent = never
