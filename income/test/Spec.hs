import Test.HUnit
import Lib

main :: IO ()
main = do
    result <- runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [
    studentLoanTests,
    nationalInsuranceTests,
    personalAllowanceTests,
    taxableIncomeTests,
    incomeTaxTests,
    totalDeductionTests,
    netIncomeTests
    ]

studentLoanTests :: Test
studentLoanTests = TestList [
    TestCase(assertEqual "No Student Loan Payment" 0 (studentLoan 25000)),
    TestCase(assertEqual "Student Loan Payment" 2044 (studentLoan 50000))
    ]

nationalInsuranceTests :: Test
nationalInsuranceTests = TestList [
    TestCase(assertEqual "No National Insurance" 0 (nationalInsurance 5000)),
    TestCase(assertEqual "12 Percent National Insurance" 1852 (nationalInsurance 25000)),
    TestCase(assertEqual "2 Percent National Insurance" 5278 (nationalInsurance 70000))
    ]

personalAllowanceTests :: Test
personalAllowanceTests = TestList [
    TestCase(assertEqual "Maximum Personal Allowance" 12570 (personalAllowance 40000)),
    TestCase(assertEqual "Lower Personal Allowance" 7570 (personalAllowance 110000)),
    TestCase(assertEqual "Zero Personal Allowance" 0 (personalAllowance 150000))
    ]

taxableIncomeTests :: Test
taxableIncomeTests = TestList [
    TestCase (assertEqual "No Taxable Income" 0 (taxableIncome 10000)),
    TestCase (assertEqual "Some Taxable Income" 37430 (taxableIncome 50000)),
    TestCase (assertEqual "Full Taxable Income" 150000 (taxableIncome 150000))
    ]

incomeTaxTests :: Test
incomeTaxTests = TestList [
    TestCase (assertEqual "No Tax" 0 (incomeTax 10000)),
    TestCase (assertEqual "Basic Tax Rate" 1486 (incomeTax 20000)),
    TestCase (assertEqual "Higher Tax Rate" 15432 (incomeTax 70000)),
    TestCase (assertEqual "Additional Tax Rate" 63710 (incomeTax 175000))
    ]

totalDeductionTests :: Test
totalDeductionTests = TestList [
    TestCase (assertEqual "Total Deductions (No SL, No NI, No IT, Full PA)" 0 (totalDeduction 5000)),
    TestCase (assertEqual "Total Deductions (No SL, Max NI, No IT, Full PA)" 52 (totalDeduction 10000)),
    TestCase (assertEqual "Total Deductions (No SL, Max NI, Min IT, Full PA)" 1138 (totalDeduction 15000)),
    TestCase (assertEqual "Total Deductions (SL, Max NI, Min IT, Full PA)" 6182 (totalDeduction 30000)),
    TestCase (assertEqual "Total Deductions (SL, Min NI, Mid IT, Full PA)" 19454 (totalDeduction 60000)),
    TestCase (assertEqual "Total Deductions (SL, Min NI, Mid IT, Decr PA)" 46954 (totalDeduction 110000)),
    TestCase (assertEqual "Total Deductions (SL, Min NI, Mid IT, No PA)" 60182 (totalDeduction 130000)),
    TestCase (assertEqual "Total Deductions (SL, Min NI, Max IT, No PA)" 81582 (totalDeduction 170000))
    ]

netIncomeTests :: Test
netIncomeTests = TestList [
    TestCase (assertEqual "Net Income (No SL, No NI, No IT, Full PA)" 5000 (netIncome 5000)),
    TestCase (assertEqual "Net Income (No SL, Max NI, No IT, Full PA)" 9948 (netIncome 10000)),
    TestCase (assertEqual "Net Income (No SL, Max NI, Min IT, Full PA)" 13862 (netIncome 15000)),
    TestCase (assertEqual "Net Income (SL, Max NI, Min IT, Full PA)" 23818 (netIncome 30000)),
    TestCase (assertEqual "Net Income (SL, Min NI, Mid IT, Full PA)" 40546 (netIncome 60000)),
    TestCase (assertEqual "Net Income (SL, Min NI, Mid IT, Decr PA)" 63046 (netIncome 110000)),
    TestCase (assertEqual "Net Income (SL, Min NI, Mid IT, No PA)" 69818 (netIncome 130000)),
    TestCase (assertEqual "Net Income (SL, Min NI, Max IT, No PA)" 88418 (netIncome 170000))
    ]