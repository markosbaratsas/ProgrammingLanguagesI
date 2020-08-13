#!/usr/bin/python
import sys

# The algorith is similar to this in:
# https://www.geeksforgeeks.org/shortest-path-in-a-binary-maze/
# but (obviously) with some adjustments...

from collections import deque

# Initialize global parameters in...
# They will be fixed accordingly later(in def openFileGetMatrix())
ROW = -1
COL = -1


# To store matrix cell cordinates
class Point:
    def __init__(self,x: int, y: int):
        self.x = x
        self.y = y

# A data structure for queue used in BFS
class queueNode:
    def __init__(self,pt: Point, dist: int):
        self.pt = pt  # The cordinates of the cell
        self.dist = dist  # Cell's distance from the source OR time needed to reach this cell...


# (kinda) noobish way of reading the file(too many ifs repeated)...
# It works they way we need it though!
def openFileGetMatrix(filename):
    # list that is going to contain all airports
    airports = []

    with open(filename, "rt") as f:
        returningList = []

        c = f.read(1)
        while c:
            listX = []

            if c == "T":
                T = Point(len(returningList), len(listX))
            elif c == "S":
                S = Point(len(returningList), len(listX))
            elif c == "W":
                W = Point(len(returningList), len(listX))
            elif c == "A":
                A = Point(len(returningList), len(listX))
                airports.append(A)

            if c != "\n":
                listX.append(c)
            c = f.read(1)
            while c and c!="\n":
                if c == "T":
                    T = Point(len(returningList), len(listX))
                elif c == "S":
                    S = Point(len(returningList), len(listX))
                elif c == "W":
                    W = Point(len(returningList), len(listX))
                elif c == "A":
                    A = Point(len(returningList), len(listX))
                    airports.append(A)

                listX.append(c)
                c = f.read(1)


            if listX != []:
                returningList.append(listX)

    # Fix values of global parameters ROW,COL
    global ROW
    global COL
    ROW = len(returningList)
    COL = len(returningList[0])

    return (returningList, S, T, W,airports)


# Check whether given cell(row,col)
# is a valid cell or not
def isValid(row: int, col: int):
    return (row >= 0) and (row < ROW) and (col >= 0) and (col < COL)

# These arrays are used to get row and column
# numbers of 4 neighbours of a given cell
# Because we need the smallest lexicographical solution
# we First go Down, then Left, then Right, then Up
rowNum = [1, 0, 0, -1]
colNum = [0, -1, 1, 0]

# We go from the destination node(T), towards the first node(S),
# by finding it's cell's previous cell...
def findPath(previousRow, previousCol, src: Point, dest:Point):
    countNumber = 0
    returningStr = []
    pointRow, pointCol = dest.x,dest.y
    while((pointRow != src.x) or (pointCol != src.y)):
        countNumber += 1
        oldPointRow = pointRow
        oldPointCol = pointCol
        pointRow, pointCol = previousRow[pointRow][pointCol],previousCol[pointRow][pointCol]

        # Keep in mind we are going in opposite direction...
        if pointRow > oldPointRow:
            returningStr.append("U")
        elif pointRow < oldPointRow:
            returningStr.append("D")
        if pointCol < oldPointCol:
            returningStr.append("R")
        elif pointCol > oldPointCol:
            returningStr.append("L")

    returningStr.reverse()
    return (returningStr, countNumber)


# This function uses BFS algorithm logic, but:
# In each iteration the person "moves" in a BFS way(in order to find the shortest root)
# AND corona moves in BFS way, keeping in mind, what we need to do if we find an airport...
def BFSstayhome(mat, person: Point, corona: Point, dest: Point, airports):

    # It tells us which cells are visited
    # Despite the fact that we color the mat[][], it's necessary
    # in order to avoid revisits...
    visitedPerson = [[False for j in range(COL)] for i in range(ROW)]
    visitedCorona = [[False for j in range(COL)] for i in range(ROW)]

    # Mark the source cell as visited
    visitedPerson[person.x][person.y] = True

    # Create a queues for the 2 BFS
    qPerson = []
    qCorona = []

    qPerson.append(deque())
    for i in range(7):
        qCorona.append(deque())

    # Distance of source cells is 0
    s = queueNode(person,0)
    t = queueNode(corona,0)
    qPerson[0].append(s) #  Enqueue source cell
    qCorona[0].append(t) #  Enqueue source cell

    # To find previous cell's row and column
    # Needed only for person, NOT for corona
    previousRow = [[-1 for j in range(COL)] for i in range(ROW)]
    previousCol = [[-1 for j in range(COL)] for i in range(ROW)]

    # To see if corona visited airports yet
    # Needed only for corona, NOT for person
    visitedAirports = False

    # "timer" in order to sync the whole procedure
    # because corona moves in different speeds than the person
    timeCount = 0

    # Do a BFS starting from source cell
    # qPerson empty -> no possible cells to visit -> mission.. IMPOSSIBLE
    while qPerson[timeCount]:
        qPerson.append(deque())
        qCorona.append(deque())

        # ------- code for corona

        # if qCorona has sth to infect in timeCount then do...
        if qCorona[timeCount]:

            # handle all corona cells that their time has come(by checking timecount)...
            for k in range(len(qCorona[timeCount])):

                currPositionCorona = qCorona[timeCount].popleft() # Dequeue the front cell

                # If we have reached the destination cell,
                # we are done
                ptCorona = currPositionCorona.pt

                # show infection on the mat
                mat[ptCorona.x][ptCorona.y] = "W"

                # If corona reached first destination
                # then... IMPOSSIBLE
                if ptCorona.x == dest.x and ptCorona.y == dest.y:
                    return ("IMPOSSIBLE", previousRow, previousCol)

                # Otherwise enqueue its adjacent cells
                for i in range(4):
                    row = ptCorona.x + rowNum[i]
                    col = ptCorona.y + colNum[i]
                    
                    # If adjacent cell is valid, is the appropriate character, enqueue it.
                    if (isValid(row,col) and (mat[row][col] == "." or mat[row][col] == "S" or mat[row][col] == "T") and not visitedCorona[row][col]):
                        visitedCorona[row][col] = True
                        Adjcell = queueNode(Point(row,col),currPositionCorona.dist+2)
                        qCorona[timeCount+2].append(Adjcell)


                    # If adjacent cell is airport, enque it and enque all airports as well...
                    elif (isValid(row,col) and (mat[row][col] == "A") and not visitedAirports and not visitedCorona[row][col]):
                        visitedAirports = True
                        visitedCorona[row][col] = True
                        Adjcell = queueNode(Point(row,col),currPositionCorona.dist+2)
                        qCorona[timeCount+2].append(Adjcell)

                        # all airports should be appended but with distance 2+5 = 7
                        for j in range(len(airports)):
                            visitedCorona[airports[j].x][airports[j].y] = True
                            Adjcell = queueNode(airports[j],currPositionCorona.dist+6)
                            qCorona[timeCount+7].append(Adjcell)


        # ------- code for person

        # handle all "person cells" that their time has come(by checking timecount)...
        for k in range(len(qPerson[timeCount])):

            currPositionPerson = qPerson[timeCount].popleft() # Dequeue the front cell

            timeCount = currPositionPerson.dist

            # If we have reached the destination cell, we are done
            ptPerson = currPositionPerson.pt
            if ptPerson.x == dest.x and ptPerson.y == dest.y:
                return (currPositionPerson.dist, previousRow, previousCol)

            # If corona reached this cell, we cant do sth..
            # Continue to next cell
            if mat[ptPerson.x][ptPerson.y] == "W":
                continue

            # Otherwise enqueue its adjacent cells
            # for loop used for person
            for i in range(4):
                row = ptPerson.x + rowNum[i]
                col = ptPerson.y + colNum[i]

                # If adjacent cell is valid, has path, and not visited yet, enqueue it.
                # We use visited, so that we do not enque more than one time the same cell
                if (isValid(row,col) and (mat[row][col] == "." or mat[row][col] == "A" or mat[row][col] == "T") and not visitedPerson[row][col]):
                    previousRow[row][col] = ptPerson.x
                    previousCol[row][col] = ptPerson.y
                    visitedPerson[row][col] = True
                    Adjcell = queueNode(Point(row,col),currPositionPerson.dist+1)
                    qPerson[timeCount+1].append(Adjcell)


        timeCount += 1


    # ------end of while


    # Outside of while... No more cells for person to visit and..
    # he didn't reach destination... --> "IMPOSSIBLE"
    return ("IMPOSSIBLE", previousRow, previousCol)

# main function
def main():
    (mat, S, T, W,airports) = openFileGetMatrix(sys.argv[1])

    source = Point(0,0)
    dest = Point(3,4)

    (personDist, previousRow, previousCol) = BFSstayhome(mat, S, W, T, airports)

    if personDist != "IMPOSSIBLE":
        (returningStr, countNumber) = findPath(previousRow, previousCol, S, T)
        print(countNumber)
        print("".join(map(str,returningStr)))
    else:
        print(personDist)



main()
