def reverse_board(board):
    return [[0 if board[i][j] == 1 else 1 if board[i][j] == 0 else None for i in range(len(board))] for j in range(len(board[0]))]


def english_board(i=3, j=3):
    init_board = [
        [None, None, 1, 1, 1, None, None],
        [None, None, 1, 1, 1, None, None],
        [1, 1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1, 1, 1],
        [None, None, 1, 1, 1, None, None],
        [None, None, 1, 1, 1, None, None]
    ]
    init_board[i][j] = 0
    return init_board, reverse_board(init_board)


def french_board(i=3, j=3):
    init_board = [
        [None, None, 1, 1, 1, None, None],
        [None, 1, 1, 1, 1, 1, None],
        [1, 1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1, 1, 1],
        [None, 1, 1, 1, 1, 1, None],
        [None, None, 1, 1, 1, None, None]
    ]
    init_board[i][j] = 0
    return init_board, reverse_board(init_board)


def order_board(n, i=0, j=0):
    init_board = [[1 for _ in range(n)] for _ in range(n)]
    init_board[i][j] = 0
    return init_board, reverse_board(init_board)


def test1():
    init_board = [
        [1, None, None],
        [1, None, None],
        [0, 1, 0]
    ]
    final_board = [
        [0, None, None],
        [0, None, None],
        [0, 0, 1]
    ]
    return init_board, final_board


def test2(i=1, j=1):
    init_board = [
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1]
    ]
    init_board[i][j] = 0
    final_board = [
        [1, 0, 1],
        [0, 0, 1],
        [0, 0, 1]
    ]
    return init_board, final_board


def generate_boards(mode, origin_x, origin_y):
    if mode == "english":
        return english_board(origin_x, origin_y)
    if mode == "french":
        return french_board(origin_x, origin_y)
    if mode == "3x3":
        return order_board(3, origin_x, origin_y)
    if mode == "4x4":
        return order_board(4, origin_x, origin_y)
    if type == "test1":
        return test1()
    return test2()
