module Lib where

--Global Variables
studentLoanThresh :: Int
studentLoanThresh = 27288

nationalInsuranceLowerCap :: Int
nationalInsuranceLowerCap = 9564

nationalInsuranceUpperCap :: Int
nationalInsuranceUpperCap = 50268

defaultPersonalAllowance :: Int
defaultPersonalAllowance = 12570

personalAllowanceDecreaseThresh :: Int
personalAllowanceDecreaseThresh = 100000

personalAllowanceEndThresh :: Int
personalAllowanceEndThresh = 125140

basicRateCap :: Int
basicRateCap = 37700

higherRateCap :: Int
higherRateCap = 150000

--Student Loan Calculator
studentLoan :: Int -> Int
studentLoan income = 
    --Check that income is below threshold and return 0
    if income < studentLoanThresh
        then 0

        --Otherwise multiply amount over threshold by 0.09
        else ((income - studentLoanThresh) * 9) `div` 100

--National Insurance Calculator
nationalInsurance :: Int -> Int
nationalInsurance income = 
    --Check that income is below threshold and return 0
    if income < nationalInsuranceLowerCap
        then 0

        --Check that income is below upper cap, and multiply amount over lower cap by 0.12
        else if income < nationalInsuranceUpperCap
            then ((income - nationalInsuranceLowerCap) * 12) `div` 100

            --Otherwise multiply amount between upper and lower cap by 0.12, and amount over upper cap by 0.02
            else ((nationalInsuranceUpperCap - nationalInsuranceLowerCap) * 12) `div` 100 
                + ((income - nationalInsuranceUpperCap) * 2) `div` 100

--_Income Tax Calculators_
--Personal Allowance Calculator
personalAllowance :: Int -> Int
personalAllowance income = 
    --Check that income is greater personal allowance end threshold and return 0
    if income >= personalAllowanceEndThresh
        then 0

        --Otherwise check that income is greater than personal allowance deduction threshold and deduct £1 for every £2 that it is over
        else if income > personalAllowanceDecreaseThresh
            then defaultPersonalAllowance - (income - personalAllowanceDecreaseThresh) `div` 2

            --Otherwise return default personal allowance value
            else defaultPersonalAllowance

--Taxable Income Calculator
taxableIncome :: Int -> Int
taxableIncome income = 
    --Check that income is less than personal allowance and return 0
    if income < personalAllowance income
        then 0

        --Otherwise deduct personal allowance from income to calculate taxable income
        else income - personalAllowance income

--Income Tax Calculator
incomeTax :: Int -> Int
incomeTax income = 
    --Check that taxable income is 0 and return 0
    if taxableIncome income == 0
        then 0

        --Otherwise check that taxable income falls within basic rate and multiply by 0.2
        else if taxableIncome income < basicRateCap
            then taxableIncome income `div` 5

            --Otherwise check that taxable income falls within higher rate and calculate full basic rate + amount over basic rate * 0.4
            else if taxableIncome income < higherRateCap
                then basicRateCap `div` 5 + ((taxableIncome income - basicRateCap) * 4) `div` 10

                --Otherwise check that taxable income is in additional rate and calculate full basic rate + full higher rate + amount over higher rate * 0.45
                else basicRateCap `div` 5 
                    + ((higherRateCap - basicRateCap) * 4) `div` 10
                    + ((taxableIncome income - higherRateCap) * 45) `div` 100

--Total Deductions Calculator
totalDeduction :: Int -> Int
totalDeduction income = 
    studentLoan income 
    + nationalInsurance income 
    + incomeTax income

--Net Income Calculator
netIncome :: Int -> Int
netIncome income = 
    income - totalDeduction income