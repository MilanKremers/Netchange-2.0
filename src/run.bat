cd "%~dp0"
stack build
start cmd /k stack run -- 1100 1102 1101
start cmd /k stack run -- 1101 1100 1102
start cmd /k stack run -- 1102 1100 1101 1106 1104
start cmd /k stack run -- 1104 1102 1105
start cmd /k stack run -- 1105 1104 1106
stack run -- 1106 1105 1102
pause