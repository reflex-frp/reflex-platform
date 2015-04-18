-- Copyright (c) David Amos, 2009. All rights reserved.

module Math.Projects.Rubik where

import Math.Algebra.Group.PermutationGroup hiding (_D)
import Math.Algebra.Group.SchreierSims as SS
import Math.Algebra.Group.RandomSchreierSims as RSS
import Math.Algebra.Group.Subquotients


-- Rubik's cube

--           11 12 13
--           14  U 16
--           17 18 19
-- 21 22 23   1  2  3  41 42 43  51 52 53
-- 24  L 26   4  F  6  44  R 46  54  B 56
-- 27 28 29   7  8  9  47 48 49  57 58 59
--           31 32 33
--           34  D 36
--           37 38 39

f = p [[ 1, 3, 9, 7],[ 2, 6, 8, 4],[17,41,33,29],[18,44,32,26],[19,47,31,23]]
b = p [[51,53,59,57],[52,56,58,54],[11,27,39,43],[12,24,38,46],[13,21,37,49]]
l = p [[21,23,29,27],[22,26,28,24],[ 1,31,59,11],[ 4,34,56,14],[ 7,37,53,17]]
r = p [[41,43,49,47],[42,46,48,44],[ 3,13,57,33],[ 6,16,54,36],[ 9,19,51,39]]
u = p [[11,13,19,17],[12,16,18,14],[ 1,21,51,41],[ 2,22,52,42],[ 3,23,53,43]]
d = p [[31,33,39,37],[32,36,38,34],[ 7,47,57,27],[ 8,48,58,28],[ 9,49,59,29]]

rubikCube = [f,b,l,r,u,d]
-- In Singmaster notation these would be capital letters.

[cornerFaces,edgeFaces] = orbits rubikCube

(kerCornerFaces,imCornerFaces) = transitiveConstituentHomomorphism rubikCube cornerFaces
-- kernel is the elts which fix all corner faces
-- image is the action restricted to the corner faces

(kerEdgeFaces,imEdgeFaces) = transitiveConstituentHomomorphism rubikCube edgeFaces
-- kernel is the elts which fix all edge faces
-- image is the action restricted to the edge faces

[cornerBlocks] = blockSystems imCornerFaces
[edgeBlocks] = blockSystems imEdgeFaces

(kerCornerBlocks,imCornerBlocks) = blockHomomorphism imCornerFaces cornerBlocks
-- kernel is elts which fix all the corners as blocks, with order 3^7
-- (Whenever you twist one corner you must untwist another
-- - so the action on 7 corners determines the 8th)
-- image is the action on the corners as blocks, which is S8 of order 20160

(kerEdgeBlocks,imEdgeBlocks) = blockHomomorphism imEdgeFaces edgeBlocks
-- kernel is elts which fix all the edges as blocks, with order 2^11
-- (Whenever you flip one edge, you must flip another edge
-- - so the action on 11 edges determines the 12th)
-- image is the action on the edges as blocks, which is S12 of order 479001600

-- Note that orderSGS imCornerFaces * orderSGS imEdgeFaces == 2 * orderSGS (sgs rubikCube)
-- This is because you can't operate on corners and edges totally independently
-- If you swap two corners, you must also swap two edges

-- See also
-- http://www.gap-system.org/Doc/Examples/rubik.html

-- (Note that the kernel of the corner constituent homomorphism /= image of edge constituent homomorphism
-- For example, [[36,38],[48,58]] is in the latter, but not the former because it's not in the Rubik group
-- ie there is an elt in the Rubik group which does just that to the edges, but may do some things to the corners)


-- Rubik's revenge (4*4*4 cube)

--                    1   2   3   4
--                    5   6   7   8
--                    9  10  11  12
--                   13  14  15  16
-- 101 102 103 104  201 202 203 204  301 302 303 304  401 402 403 404
-- 105 106 107 108  205 206 207 208  305 306 307 308  405 406 407 408
-- 109 110 111 112  209 210 211 212  309 310 311 312  409 410 411 412
-- 113 114 115 116  213 214 215 216  313 314 315 316  413 414 415 416
--                  501 502 503 504
--                  505 506 507 508
--                  509 510 511 512
--                  513 514 515 516

_U = p [[1,13,16,4],[2,9,15,8],[3,5,14,12],[6,10,11,7],
        [101,201,301,401],[102,202,302,402],[103,203,303,403],[104,204,304,404]]
_u = p [[105,205,305,405],[106,206,306,406],[107,207,307,407],[108,208,308,408]]
_d = p [[109,209,309,409],[110,210,310,410],[111,211,311,411],[112,212,312,412]]
_D = p [[113,213,313,413],[114,214,314,414],[115,215,315,415],[116,216,316,416],
        [501,504,516,513],[502,508,515,509],[503,512,514,505],[506,507,511,510]]

bf = p [[1,304,516,113],[2,308,515,109],[3,312,514,105],[4,316,513,101],
        [5,303,512,114],[6,307,511,110],[7,311,510,106],[8,315,509,102],
        [9,302,508,115],[10,306,507,111],[11,310,506,107],[12,314,505,103],
        [13,301,504,116],[14,305,503,112],[15,309,502,108],[16,313,501,104],
        [201,204,216,213],[202,208,215,209],[203,212,214,205],[206,207,211,210],
        [401,413,416,404],[402,409,415,408],[403,405,414,412],[406,410,411,407]]

_R = _U ~^ bf
_r = _u ~^ bf
_l = _d ~^ bf
_L = _D ~^ bf

ud = _U * _u * _d * _D

_B = _R ~^ ud
_b = _r ~^ ud
_f = _l ~^ ud
_F = _L ~^ ud

-- Note that orderSGS $ sgs [_U,_u,_d,_D,bf] comes out much too large,
-- because it includes rotations of the whole cube (24)
-- and exchanges of indistinguishable centre faces (24 for each of 6 colours)
-- So we have to divide by 24^7 / 2. 
-- (The /2 is because we can only have even permutations when exchanging indistinguishable centres)