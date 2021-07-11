import * as React from 'react'

import { ExpressionResult } from '../../reducer/editor/reducer'
import { ListBindings } from '../ListBindings'
import { UnitTest } from '../UnitTest'
import { ListTests } from '../ListTests'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { ExprHash } from '../../types'
import { Compile } from './Compile'
import { Deploy } from './Deploy'

type Props = {
  projectHash: ExprHash
  result: ExpressionResult
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  onFetchExpressionsForHashes: (hashes: ExprHash[]) => void
}

export const Feedback: React.FC<Props> = ({
  result,
  onBindingSelect,
  onFetchExpressionsForHashes,
}) => {
  switch (result.type) {
    case 'ShowError':
      return <Paragraph>{result.error}</Paragraph>
    case 'ShowEvaluate':
      return (
        <FlexColumnSpaced>
          {/*<Paragraph>#{result.expression.edHash}</Paragraph>*/}
          <Code>{result.evaluatedValue}</Code>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          <ListTests
            unitTests={result.expression.edUnitTests}
          />
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )
    case 'ShowUpdatedBinding':
      // need to return new bindings and typeBindings
      const updatedMsg =
        result.updatedTestsCount > 0
          ? `${result.updatedTestsCount} tests updated`
          : null
      return (
        <FlexColumnSpaced>
          <Paragraph>{`🐴 Updated ${result.bindingName}`}</Paragraph>
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          {updatedMsg && (
            <Paragraph>{updatedMsg}</Paragraph>
          )}
          <ListTests
            unitTests={result.expression.edUnitTests}
          />
          {Object.values(result.expression.edRuntimes).map(
            rt => (
              <>
                <Compile
                  exprHash={result.expression.edHash}
                  runtime={rt.rtdName}
                  title={rt.rtdName}
                />
                {rt.rtdName === 'task-server' ||
                  (rt.rtdName === 'redis-task-server' && (
                    <Deploy
                      exprHash={result.expression.edHash}
                      title={result.bindingName}
                    />
                  ))}
              </>
            )
          )}
          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )

    case 'ShowBinding':
      return (
        <FlexColumnSpaced>
          {/*<Paragraph>
            🐴 {result.expression.edHash}
          </Paragraph>*/}
          <Code codeType="type">
            {result.expression.edType}
          </Code>
          <ListTests
            unitTests={result.expression.edUnitTests}
          />
          {Object.values(result.expression.edRuntimes).map(
            rt => (
              <FlexColumnSpaced>
                <Compile
                  exprHash={result.expression.edHash}
                  runtime={rt.rtdName}
                  title={rt.rtdName}
                />
                {rt.rtdName === 'task-server' ||
                  (rt.rtdName === 'redis-task-server' && (
                    <Deploy
                      exprHash={result.expression.edHash}
                      title={rt.rtdName}
                    />
                  ))}
              </FlexColumnSpaced>
            )
          )}

          <ListBindings
            values={result.expression.edBindings}
            types={result.expression.edTypeBindings}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )
    case 'EvaluationError':
      return (
        <FlexColumnSpaced>
          <Paragraph>
            Evaluation error, please try again
          </Paragraph>
        </FlexColumnSpaced>
      )
    case 'ShowUnitTest':
      return (
        <FlexColumnSpaced>
          <Paragraph>Test created</Paragraph>
          <UnitTest unitTest={result.unitTest} />
          <ListBindings
            values={result.unitTest.utdBindings}
            types={{}}
            onBindingSelect={onBindingSelect}
            onFetchExpressionsForHashes={
              onFetchExpressionsForHashes
            }
          />
        </FlexColumnSpaced>
      )
    case 'EditorNew':
      return <FlexColumnSpaced />
  }
}
