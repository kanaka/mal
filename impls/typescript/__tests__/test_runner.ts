import { readStr } from "../reader"
import * as fs from "fs"
import { idText } from "typescript"
import { listenerCount } from "process"


function getInputs(): [number, number, boolean] {
  let testStep = 1
  let fileNumber = 1
  let deferrable = false
  let testStepArr = process.argv.filter((x) => x.startsWith('--testStepNumber='))
  let fileNumberArr = process.argv.filter((x) => x.startsWith('--implementationStepNumber='))
  let deferrableArr = process.argv.filter((x) => x.startsWith('--deferrable='))
  if (testStepArr.length === 0 || fileNumberArr.length === 0 || deferrableArr.length === 0) {
    throw new Error("Usage: npm test __tests__/test_runner.ts -- --testStepNumber=[number] --implementationStepNumber=[number] --deferrable=[true|false]")
  } else {
    testStep = parseInt(testStepArr[0].split("=")[1].trim())
    fileNumber = parseInt(fileNumberArr[0].split("=")[1].trim())
    deferrable = fileNumberArr[0].split("=")[1].trim() === "true"
  }
  return [testStep, fileNumber, deferrable]
}

const stepNames = ["step0_repl", "step1_read_print",
"step2_eval", "step3_env",
"step4_if_fn_do", "step5_tco",
"step6_file", "step7_quote",
"step8_macros", "step9_try",
"stepA_repl"]

function getDataPath(step: number): string {
  return "../../tests/" + stepNames[step] + ".mal"
}
function getImpFilePath(step: number): string {
  return "../" + stepNames[step]
}
function linesInFile(filePath: string): string[] {
  const allFileContents = fs.readFileSync(filePath, 'utf-8');
  return allFileContents.split(/\r?\n/)
}


function generateSituations(filePath: string, deferrable: boolean): string[][] {
  const lines = linesInFile(filePath)
  const situations: string[][] = []
  for (const line of lines) {

    if (line.startsWith(";;") && line.includes("Deferrable") && !deferrable) {
      break
    } else if (line.startsWith(";;") || line === ""){
      continue
    } else if (line.startsWith(";=>")){
      const expectedOutput = line.slice(3, line.length)
      situations[situations.length-1].push(expectedOutput)
    } else if (!line.startsWith(";")) {
      const input = line
      situations.push([input])
    }
  }
  return situations
}

const [testStep, fileNumber, deferrable] = getInputs()
describe(`testing step ${testStep} on file ${stepNames[fileNumber]}.ts`, () => {
  const filePath = getImpFilePath(fileNumber)
  const lisp = require(filePath)
  const situations = generateSituations(getDataPath(testStep), deferrable)
  const mockEnv = lisp.createEnv()
  situations.forEach(situation => {
    const input = situation[0]
    const expectedOutput = situation.length > 1 ? situation[1]: null
    it(`should return ${expectedOutput} when given ${input}`, () => {
      if (expectedOutput === null) {
        lisp.REP(input, mockEnv)
      } else{      
        expect(lisp.REP(input, mockEnv)).toBe(expectedOutput)
      }
    })
  })

})