
enum LineType { case NONE, HORIZONTAL, VERTICAL }

struct Line: Hashable, Equatable, Comparable {
    
    var lineIndex: Int
    var lineType: LineType
    var coord: Set<Int> = []
    
    init(_ lineIndex: Int, _ lineType: LineType) {
        self.lineIndex = lineIndex
        self.lineType = lineType
    }
    
    func isHorizontal() -> Bool {
        lineType == LineType.HORIZONTAL
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(lineIndex)
        hasher.combine(lineType)
    }
    
    static func == (lhs: Line, rhs: Line) -> Bool {
        lhs.lineType == rhs.lineType && lhs.lineIndex == rhs.lineIndex
    }
    
    static func < (lhs: Line, rhs: Line) -> Bool {
        lhs.coord.count < rhs.coord.count
    }
}

func modifiedGetMinLines(_ matrix: [[Int]]) -> Set<Line> { // O(N^4)

    // Using the algorithm found here - https://www.youtube.com/watch?v=rrfFTdO2Z7I
    func drawLinesWhileIsolatedZerosExist(_ matrix: inout [[Int]]) -> Set<Line> { // O(N^3)

        let N = matrix.count

        var lines: Set<Line> = []
        var unprocessedTableChange = true
        while unprocessedTableChange { // While loop occurs 2N-1 times max!...each time a line in a matrix must be crossed out to continue
            unprocessedTableChange = false

            for i in 0..<N { // rows
                var zeroCount = 0
                var columnOfLastZero = -1
                for j in 0..<N {
                    if matrix[i][j] == 0 {
                        zeroCount += 1
                        columnOfLastZero = j
                    }
                }

                if zeroCount == 1 {
                    unprocessedTableChange = true

                    var selectedCol = Line(columnOfLastZero, .VERTICAL)
                    for i in 0..<N {
                        if matrix[i][columnOfLastZero] == 0 {
                            selectedCol.coord.insert(i)
                        }
                        matrix[i][columnOfLastZero] = -1 // Cross line out
                    }
                    lines.insert(selectedCol)
                }
            }

            for i in 0..<N { // columns
                var zeroCount = 0
                var rowOfLastZero = -1
                for j in 0..<N {
                    if matrix[j][i] == 0 {
                        zeroCount += 1
                        rowOfLastZero = j
                    }
                }

                if zeroCount == 1 {
                    unprocessedTableChange = true

                    var selectedRow = Line(rowOfLastZero, .HORIZONTAL)
                    for i in 0..<N {
                        if matrix[rowOfLastZero][i] == 0 {
                            selectedRow.coord.insert(i)
                        }
                        matrix[rowOfLastZero][i] = -1 // Cross line out
                    }
                    lines.insert(selectedRow)
                }
            }
        }

        return lines
    }

    func zerosToProcessExist(_ array: [Int]) -> Bool { // O(N)
        for e in array {
            if e > 0 { return true }
        }
        return false
    }

    var matrix = matrix
    let N = matrix.count
    var lines: Set<Line> = drawLinesWhileIsolatedZerosExist(&matrix) // O(N^3)
    var zerosPerRow = Array(repeating: 0, count: N)
    var zerosPerCol = Array(repeating: 0, count: N)

    for i in 0..<N { // O(N^2)
        for j in 0..<N {
            if matrix[i][j] == 0 {
                zerosPerRow[i] += 1
                zerosPerCol[j] += 1
            }
        }
    }

    while zerosToProcessExist(zerosPerRow) || zerosToProcessExist(zerosPerCol) { // While loop occurs 2N-1 times max!...each time a line in a matrix must be crossed out to continue

        var max = 0
        var lineWithMostZeros: Line?

        var linesWithMaxZeros: Set<Line> = []
        for i in 0..<N { // O(N)
            if zerosPerRow[i] > max {
                linesWithMaxZeros = []
                linesWithMaxZeros.insert(Line(i, LineType.HORIZONTAL))
                max = zerosPerRow[i]
            } else if zerosPerRow[i] == max && max > 0 {
                linesWithMaxZeros.insert(Line(i, LineType.HORIZONTAL))
            }

            if zerosPerCol[i] > max {
                linesWithMaxZeros = []
                linesWithMaxZeros.insert(Line(i, LineType.VERTICAL))
                max = zerosPerCol[i]
            } else if zerosPerCol[i] == max && max > 0 {
                linesWithMaxZeros.insert(Line(i, LineType.VERTICAL))
            }
        }

        if linesWithMaxZeros.count == 1 {
            lineWithMostZeros = linesWithMaxZeros.first
        } else {
            var minScore = Int.max
            var minScoreLine: Line?
            for l in linesWithMaxZeros {
                var score = 0
                if l.isHorizontal() {
                    for j in 0..<N {
                        if matrix[l.lineIndex][j] == 0 {
                            for k in 0..<N {
                                if matrix[k][j] == 0 { score += 1 }
                            }
                        }
                    }
                } else {
                    for j in 0..<N {
                        if matrix[j][l.lineIndex] == 0 {
                            for k in 0..<N {
                                if matrix[j][k] == 0 { score += 1 }
                            }
                        }
                    }
                }
                if score < minScore {
                    minScore = score
                    minScoreLine = l
                }
            }
            lineWithMostZeros = minScoreLine
        }

        let index = lineWithMostZeros!.lineIndex
        var temp: Set<Int> = []
        if lineWithMostZeros!.isHorizontal() { // O(N)
            zerosPerRow[index] = 0
            for j in 0..<N {
                if matrix[index][j] == 0 {
                    zerosPerCol[j] -= 1
                    temp.insert(j)
                }
                matrix[index][j] = -1
            }
        } else {
            zerosPerCol[index] = 0
            for j in 0..<N {
                if matrix[j][index] == 0 {
                    zerosPerRow[j] -= 1
                    temp.insert(j)
                }
                matrix[j][index] = -1
            }
        }
        lineWithMostZeros!.coord = temp
        lines.insert(lineWithMostZeros!)
    }
    return lines
}

func hungarianAlgorithm( _ table: [[Int]]) -> Int {
    var tempTable = table
    var n = table.count
    // Row reduction - O(n^2)
    for i in 0..<n {
        let minimum = tempTable[i].min()!
        for j in 0..<n {
            tempTable[i][j] -= minimum
        }
    }
    // Column reduction - O(n^2)
    for i in 0..<tempTable.count {
        var minimum = Int.max
        for j in 0..<n {
            minimum = min(minimum, tempTable[j][i])
        }
        for j in 0..<n {
            tempTable[j][i] -= minimum
        }
    }
    // Test for optimal assignment - O(n^3)
    n = tempTable.count
    var lines: Set<Line>!
    loop: while true {
        lines = modifiedGetMinLines(tempTable)
        if lines.count >= n { break loop } // Optimal assignment can be made
        // Shift zeros - O(n^3)
        var minimum = Int.max
        for i in 0..<n {
            for j in 0..<n {
                if !lines.contains(Line(i, .HORIZONTAL)) && !lines.contains(Line(j, .VERTICAL)) {
                    minimum = min(minimum, tempTable[i][j])
                }
            }
        }
        for i in 0..<n {
            for j in 0..<n {
                if !lines.contains(Line(i, .HORIZONTAL)) && !lines.contains(Line(j, .VERTICAL)) {
                    tempTable[i][j] -= minimum
                } else if lines.contains(Line(i, .HORIZONTAL)) && lines.contains(Line(j, .VERTICAL)) {
                    tempTable[i][j] += minimum
                }
            }
        }
    }
    // Make final assignment - O(n)
    var assignments: [Int] = []
    var prevState: [([Int], Set<Line>)] = []
    while assignments.count != n && !lines.isEmpty {
        if getNextAssignment(&assignments,&lines,&prevState) != nil || lines.isEmpty && assignments.count != n {
            let (prevAssignments,prevLines) = prevState.removeLast()
            assignments = prevAssignments
            lines = prevLines
        }
    }
    var sum = 0
    for c in assignments {
        sum += table[c/100][c%100]
    }
    return sum
}

func getNextAssignment( _ assignments: inout [Int], _ lines: inout Set<Line>, _ prevState: inout [([Int], Set<Line>)]) -> String? {
    
    let sortedLines = Array(lines).sorted()
    
    guard let selectedLine = sortedLines.first else {
        return "sortedLines.first is NIL"
    }
    
    guard let coord = selectedLine.coord.first else {
        return "selectedLine.coord.first is NIL"
    }
    
    if selectedLine.coord.count > 1 {
        var prevLines: Set<Line> = []
        if sortedLines.count > 1{
            for i in 1..<sortedLines.count {
                prevLines.insert(sortedLines[i])
            }
        }
        
        var firstLine = selectedLine
        var firstCoords = selectedLine.coord
        firstCoords.remove(coord)
        for c in firstCoords {
            firstLine.coord = [c]
            
            var linesToStore = prevLines
            linesToStore.insert(firstLine)
            prevState.append((assignments,linesToStore))
        }
    }
    
    let rowToRemove = selectedLine.isHorizontal()  ? selectedLine.lineIndex : coord
    let colToRemove = selectedLine.isHorizontal()  ? coord : selectedLine.lineIndex
    
    var newLines: Set<Line> = []
    for line in lines {
        if line.isHorizontal() && line.lineIndex == rowToRemove || !line.isHorizontal() && line.lineIndex == colToRemove { continue }
        
        var tempLine = line
        tempLine.coord.remove(line.isHorizontal() ? colToRemove : rowToRemove)
        newLines.insert(tempLine)
    }
    lines = newLines
    
    assignments.append(rowToRemove * 100 + colToRemove)
    
    return nil
}
