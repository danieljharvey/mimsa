import * as O from 'fp-ts/Option'
import { State } from '../types'
import { pipe } from 'fp-ts/function'
import { ExprHash, ExpressionData } from '../../types'

export const safeSessionStorageGet = (
  key: string
): O.Option<string> =>
  O.fromNullable(sessionStorage.getItem(key))

export const safeDecode = <A>(str: string): O.Option<A> => {
  try {
    const a = JSON.parse(str)
    return O.some(a)
  } catch {
    return O.none
  }
}

const sessionStorageKey = 'project'

export type Project = { hash: ExprHash }

export const projectSet = (project: Project) =>
  sessionStorage.setItem(
    sessionStorageKey,
    JSON.stringify(project)
  )

export const projectGet = (): O.Option<Project> =>
  pipe(
    safeSessionStorageGet(sessionStorageKey),
    O.chain((str) => safeDecode<Project>(str))
  )

export const findExpressionForBinding = (
  bindingName: string,
  state: State
): O.Option<ExpressionData> =>
  pipe(
    O.fromNullable(state.project.bindings[bindingName]),
    O.chain((exprHash) =>
      O.fromNullable(state.project.store[exprHash])
    )
  )

export const findExpressionForTypeBinding = (
  bindingName: string,
  state: State
): O.Option<ExpressionData> =>
  pipe(
    O.fromNullable(state.project.typeBindings[bindingName]),
    O.chain((exprHash) =>
      O.fromNullable(state.project.store[exprHash])
    )
  )

export const findExpressionForAnyBinding = (
  bindingName: string,
  state: State
): O.Option<ExpressionData> =>
  pipe(
    findExpressionForBinding(bindingName, state),
    O.alt(() =>
      findExpressionForTypeBinding(bindingName, state)
    )
  )

export const findExpression = (
  exprHash: string,
  state: State
): O.Option<ExpressionData> =>
  O.fromNullable(state.project.store[exprHash])
