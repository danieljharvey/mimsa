import { Lens } from 'monocle-ts'
import { State } from '../types'
import {
  EventReducer,
  stateOnly,
  stateAndEvent,
  stateAndEvents,
} from '../../utils/useEventReducer'
import { ProjectAction, ProjectEvent } from './types'

import {
  listBindings,
  createProject,
  fetchExpressions,
  saveToSessionStorage,
} from './events'
export * from './events'
export * from './actions'
export * from './types'

const projectL = Lens.fromProp<State>()('project')

export const projectReducer: EventReducer<
  State,
  ProjectAction,
  ProjectEvent
> = (state, action) => {
  switch (action.type) {
    case 'Initialise':
      // attempt to find project in session storage
      return stateAndEvent(
        state,
        listBindings(state.project.projectHash)
      )
    case 'CreateProject':
      // create new project
      return stateAndEvent(state, createProject())

    case 'StoreProjectData':
      // store new bindings, fetching new expressions
      return stateAndEvents(
        projectL.set({
          ...state.project,
          projectHash: action.data.pdHash,
          bindings: action.data.pdBindings,
          typeBindings: action.data.pdTypeBindings,
        })(state),
        [
          fetchExpressions(
            [
              ...Object.values(action.data.pdBindings),
              ...Object.values(action.data.pdTypeBindings),
              ...action.extraHashes,
            ],
            action.data.pdHash
          ),
          saveToSessionStorage(action.data.pdHash),
        ]
      )

    case 'FetchExpressionsForHashes':
      // fetch new expressions (used for fetching exprs that may not have a top level binding)
      return stateAndEvents(state, [
        fetchExpressions(
          action.hashes,
          state.project.projectHash
        ),
      ])
    case 'StoreProjectHash':
      // save the project hash only, triggering fetch of project data
      return stateAndEvent(
        projectL.set({
          ...state.project,
          projectHash: action.projectHash,
        })(state),
        listBindings(action.projectHash)
      )

    case 'FetchExpressionSuccess':
      return stateOnly(
        projectL.set({
          ...state.project,
          store: {
            ...state.project.store,
            [action.exprHash]: action.storeExpression,
          },
        })(state)
      )

    default:
      return stateOnly(state)
  }
}
