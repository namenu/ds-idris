module Transform

import Mat4

%access export

translation : Double -> Double -> Double -> Mat4
translation x y z = [[1, 0, 0, x],
                     [0, 1, 0, y],
                     [0, 0, 1, z],
                     [0, 0, 0, 1]]

scaling : Double -> Double -> Double -> Mat4
scaling x y z = [[x, 0, 0, 0],
                 [0, y, 0, 0],
                 [0, 0, z, 0],
                 [0, 0, 0, 1]]

rotation_x : Double -> Mat4
rotation_x r = let cosr = cos r
                   sinr = sin r
                in [[1, 0, 0, 0],
                     [0, cosr, -sinr, 0],
                     [0, sinr, cosr, 0],
                     [0, 0, 0, 1]]

rotation_y : Double -> Mat4
rotation_y r = let cosr = cos r
                   sinr = sin r
                in [[cosr, 0, sinr, 0],
                    [0, 1, 0, 0],
                    [-sinr, 0, cosr, 0],
                    [0, 0, 0, 1]]

rotation_z : Double -> Mat4
rotation_z r = let cosr = cos r
                   sinr = sin r
                in [[cosr, -sinr, 0, 0],
                    [sinr, cosr, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]]

shearing : Double -> Double -> Double -> Double -> Double -> Double -> Mat4
shearing xy xz yx yz zx zy = [[ 1, xy, xz, 0],
                              [yx,  1, yz, 0],
                              [zx, zy,  1, 0],
                              [ 0,  0,  0, 1]]
