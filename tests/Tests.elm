module Tests exposing (suite)

import Test exposing (Test, describe, test)
import Expect
import Math.Vector2 as V2
import Math.Vector3 as V3
import Math.Vector4 as V4
import Math.Matrix4 as M4


suite : Test
suite =
    describe "All tests"
        [ v2Suite
        , m4Suite
        ]


v2Suite : Test
v2Suite =
    describe "Vector2 module"
        [ test "vec2" <|
            \() ->
                Expect.equal
                    (V2.vec2 3 4)
                    (V2.vec2 3 4)
        , test "setX" <|
            \() ->
                Expect.equal
                    (V2.vec2 5 4)
                    (V2.vec2 3 4 |> V2.setX 5)
        , test "setY" <|
            \() ->
                Expect.equal
                    (V2.vec2 3 6)
                    (V2.vec2 3 4 |> V2.setY 6)
        ]


m4Suite : Test
m4Suite =
    describe "Matrix4 module"
        [ test "inverse" <|
            \() ->
                let
                    m =
                        M4.makeRotate 1 (V3.vec3 3 4 5)
                in
                    expectMatrixEqual 1.0e-6
                        (M4.mul m (M4.inverse m))
                        M4.identity
        , test "fromColumns vs identity" <|
            \() ->
                Expect.equal
                    (M4.fromColumns ( V4.vec4 1 0 0 0, V4.vec4 0 1 0 0, V4.vec4 0 0 1 0, V4.vec4 0 0 0 1 ))
                    M4.identity
        , test "fromColumns vs transpose" <|
            \() ->
                Expect.equal
                    (M4.fromColumns ( V4.vec4 1 2 3 4, V4.vec4 5 6 7 8, V4.vec4 9 10 11 12, V4.vec4 13 14 15 16 ))
                    (M4.fromColumns ( V4.vec4 1 5 9 13, V4.vec4 2 6 10 14, V4.vec4 3 7 11 15, V4.vec4 4 8 12 16 )
                        |> M4.transpose
                    )
        , test "toColumns" <|
            \() ->
                Expect.equal
                    (M4.fromColumns ( V4.vec4 1 2 3 4, V4.vec4 5 6 7 8, V4.vec4 9 10 11 12, V4.vec4 13 14 15 16 )
                        |> M4.toColumns
                    )
                    ( V4.vec4 1 2 3 4, V4.vec4 5 6 7 8, V4.vec4 9 10 11 12, V4.vec4 13 14 15 16 )
        ]


floatEqual : Float -> Float -> Float -> Expect.Expectation
floatEqual epsilon f1 f2 =
    if abs (f1 - f2) <= epsilon then
        Expect.pass
    else
        Expect.fail (toString f1 ++ " vs " ++ toString f2)


{-| Chain together two Expectations, returning first failure if any. This does
    not do lazy-evaluation; both Expectations are evaluated in any case.
-}
passAndThen : Expect.Expectation -> Expect.Expectation -> Expect.Expectation
passAndThen expect2 expect1 =
    case Expect.getFailure expect1 of
        Nothing ->
            expect2

        Just { message } ->
            Expect.fail message


failMap : (String -> String) -> Expect.Expectation -> Expect.Expectation
failMap messageUpdater expect =
    case Expect.getFailure expect of
        Nothing ->
            expect

        Just { message } ->
            Expect.fail (messageUpdater message)


vector4Equal : Float -> V4.Vec4 -> V4.Vec4 -> Expect.Expectation
vector4Equal epsilon v1 v2 =
    let
        ( f11, f12, f13, f14 ) =
            V4.toTuple v1

        ( f21, f22, f23, f24 ) =
            V4.toTuple v2
    in
        floatEqual epsilon f11 f21
            |> passAndThen (floatEqual epsilon f12 f22)
            |> passAndThen (floatEqual epsilon f13 f23)
            |> passAndThen (floatEqual epsilon f14 f24)


expectMatrixEqual : Float -> M4.Mat4 -> M4.Mat4 -> Expect.Expectation
expectMatrixEqual epsilon m1 m2 =
    let
        ( c11, c12, c13, c14 ) =
            M4.toColumns m1

        ( c21, c22, c23, c24 ) =
            M4.toColumns m2
    in
        vector4Equal epsilon c11 c21
            |> passAndThen (vector4Equal epsilon c12 c22)
            |> passAndThen (vector4Equal epsilon c13 c23)
            |> passAndThen (vector4Equal epsilon c14 c24)
            |> failMap (\msg -> msg ++ "\nin\n" ++ toString m1 ++ "\nvs\n" ++ toString m2)
