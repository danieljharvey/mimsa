import {
  ErrorLocation,
  SourceItem,
  TypedHoleResponse,
} from '../../../types'
import * as Arr from 'fp-ts/Array'
import * as O from 'fp-ts/Option'
import {
  Position,
  editor,
} from 'monaco-editor/esm/vs/editor/editor.api'

const sourceSize = ({
  ssRowStart,
  ssRowEnd,
  ssColStart,
  ssColEnd,
}: SourceItem['siSourceSpan']): number => {
  const width = Math.max(ssRowEnd - ssRowStart, 1)
  const height = Math.max(ssColEnd - ssColStart, 1)
  return width * height
}

export const chooseSourceSpan = (
  sourceItems: SourceItem[],
  position: Position
): O.Option<SourceItem> =>
  Arr.head(
    sourceItems
      .filter(
        ({
          siSourceSpan: {
            ssRowStart,
            ssRowEnd,
            ssColStart,
            ssColEnd,
          },
        }) =>
          position.lineNumber >= ssRowStart &&
          position.lineNumber <= ssRowEnd &&
          position.column >= ssColStart &&
          position.column <= ssColEnd
      )
      .sort(
        (a, b) =>
          sourceSize(a.siSourceSpan) -
          sourceSize(b.siSourceSpan)
      )
  )

export const createMarkerForTypedHole = ({
  thMonoType,
  thSourceSpan,
}: TypedHoleResponse): editor.IMarkerData => ({
  severity: 1, // hint,
  message: `Inferred type: ${thMonoType}`,
  startLineNumber: thSourceSpan.ssRowStart,
  endLineNumber: thSourceSpan.ssRowEnd,
  startColumn: thSourceSpan.ssColStart + 1,
  endColumn: thSourceSpan.ssColEnd + 1,
})

export const createMarkerForError = ({
  elSourceSpan,
}: ErrorLocation): editor.IMarkerData => ({
  severity: 8, // error,
  message: 'Error!',
  startLineNumber: elSourceSpan.ssRowStart,
  endLineNumber: elSourceSpan.ssRowEnd,
  startColumn: elSourceSpan.ssColStart + 1,
  endColumn: elSourceSpan.ssColEnd + 1,
})