import * as React from 'react'
import { UnitTestData } from '../types/'
import { UnitTest } from './UnitTest'
import { Paragraph } from './View/Paragraph'

type Props = {
  unitTests: UnitTestData[]
}

export const ListTests: React.FC<Props> = ({
  unitTests,
}) => {
  const passing = unitTests.filter(
    (ut) => ut.utdTestSuccess
  )
  const failing = unitTests.filter(
    (ut) => !ut.utdTestSuccess
  )

  if (unitTests.length === 0) {
    return null
  }

  const message = `${passing.length}/${unitTests.length} tests pass`
  return (
    <section className="list-tests">
      <Paragraph>{message}</Paragraph>
      <>
        {failing.map((unitTest, key) => (
          <UnitTest unitTest={unitTest} key={key} />
        ))}
        {passing.map((unitTest, key) => (
          <UnitTest unitTest={unitTest} key={key} />
        ))}
      </>
    </section>
  )
}
