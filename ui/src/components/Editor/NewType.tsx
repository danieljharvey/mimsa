import * as React from 'react'
import {
  State,
  Action,
  EditorState,
} from '../../reducer/types'
import { storeProjectData } from '../../reducer/project/reducer'

import { pipe } from 'fp-ts/function'
import { CodeEditor } from './CodeEditor'
import { Feedback } from './Feedback'
import { Code } from '../View/Code'
import { Panel } from '../View/Panel'
import { useAddType } from '../../hooks/useAddType'
import { fold } from '@devexperts/remote-data-ts'
import { Link } from '../View/Link'
import { Button } from '../View/Button'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { Paragraph } from '../View/Paragraph'
import { ListBindings } from '../ListBindings'
import { InlineSpaced } from '../View/InlineSpaced'
import { ExprHash } from '../../types'
import { flow } from 'fp-ts/function'
import { fetchExpressionsForHashes } from '../../reducer/project/actions'

type Props = {
  state: State
  dispatch: (a: Action) => void
  editor: EditorState
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const NewType: React.FC<Props> = ({
  state,
  dispatch,
  editor,
  onBindingSelect,
}) => {
  const projectHash = state.project.projectHash

  const [addNewType, typeState] = useAddType(
    projectHash,
    editor.code,
    (pd) => dispatch(storeProjectData(pd))
  )

  const onCodeChange = (a: string) =>
    dispatch({ type: 'UpdateCode', text: a })

  const onFetchExpressionsForHashes = flow(
    fetchExpressionsForHashes,
    dispatch
  )
  const { expression, code } = editor

  return (
    <>
      {pipe(
        typeState,
        fold(
          () => (
            <>
              <Panel flexGrow={2}>
                <CodeEditor
                  code={code}
                  setCode={onCodeChange}
                />
              </Panel>
              <Panel>
                <FlexColumnSpaced>
                  <Feedback
                    result={expression}
                    onBindingSelect={onBindingSelect}
                    onFetchExpressionsForHashes={
                      onFetchExpressionsForHashes
                    }
                    projectHash={projectHash}
                  />
                  {editor.stale && (
                    <Button onClick={addNewType}>
                      Create
                    </Button>
                  )}
                </FlexColumnSpaced>
              </Panel>
            </>
          ),
          () => <p>loading</p>,
          (err) => (
            <>
              <Panel flexGrow={2}>
                <CodeEditor
                  code={code}
                  setCode={onCodeChange}
                />
              </Panel>
              <Panel>
                {editor.stale && (
                  <Button onClick={addNewType}>
                    Create
                  </Button>
                )}
                <Code>{err}</Code>
              </Panel>
            </>
          ),
          (addType) => (
            <Panel>
              <FlexColumnSpaced>
                <Paragraph>{`New type added: ${addType.typeName}`}</Paragraph>
                <Code>{addType.dataTypePretty}</Code>
                <Paragraph>Typeclasses:</Paragraph>
                <InlineSpaced>
                  {addType.typeclasses.map((a) => (
                    <Link onClick={() => console.log(a)}>
                      {a}
                    </Link>
                  ))}
                </InlineSpaced>
                <Paragraph>Generated functions:</Paragraph>
                <ListBindings
                  values={addType.bindings}
                  types={addType.typeBindings}
                  onBindingSelect={onBindingSelect}
                  onFetchExpressionsForHashes={
                    onFetchExpressionsForHashes
                  }
                />
              </FlexColumnSpaced>
            </Panel>
          )
        )
      )}
    </>
  )
}
