import * as O from 'fp-ts/lib/Option'
import { State } from '../types'
import { Screen } from './types'
import * as NE from 'fp-ts/NonEmptyArray'
import { pipe } from 'fp-ts/function'

// get stack[1] if it exists
export const getLastScreen = (state: State): O.Option<Screen> =>
    pipe(NE.tail(state.view.stack), NE.fromArray, O.map(NE.head))

// get stack[0] (which always exists)
export const getCurrentScreen = (state: State): Screen =>
    NE.head(state.view.stack)
