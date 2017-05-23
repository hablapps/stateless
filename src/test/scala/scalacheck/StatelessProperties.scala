package org.hablapps.stateless
package scalacheck

import org.scalacheck._, Prop.forAll

import scalaz.Equal

object StatelessProperties {

  private def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }

  /* raw representation bindings */

  object raw {
    import core.raw._

    object foldAlg {

      def getGet[P[_], A](implicit
          alg: FoldAlg[P, A],
          eq: Equal[P[(List[A], List[A])]]) =
        forAll((_: Unit) => alg.foldAlgLaw.getGet)

      def laws[P[_], A](implicit
          alg: FoldAlg[P, A],
          eq: Equal[P[(List[A], List[A])]]): Properties =
        newProperties("foldAlg") { p =>
          p.property("getGet") = getGet[P, A]
        }
    }

    object lensAlg {

      def getGet[P[_], A](implicit
          alg: LensAlg[P, A],
          eq: Equal[P[(A, A)]]) =
        forAll((_: Unit) => alg.lensAlgLaw.getGet)

      def getPut[P[_], A](implicit
          alg: LensAlg[P, A],
          eq: Equal[P[Unit]]) =
        forAll((_: Unit) => alg.lensAlgLaw.getPut)

      def putGet[P[_], A](implicit
          alg: LensAlg[P, A],
          eq: Equal[P[A]],
          aa: Arbitrary[A]) =
        forAll(a => alg.lensAlgLaw.putGet(a))

      def putPut[P[_], A](implicit
          alg: LensAlg[P, A],
          eq: Equal[P[Unit]],
          aa: Arbitrary[A]) =
        forAll((a1, a2) => alg.lensAlgLaw.putPut(a1, a2))

      def laws[P[_], A](implicit
          alg: LensAlg[P, A],
          eq0: Equal[P[(A, A)]],
          eq1: Equal[P[Unit]],
          eq2: Equal[P[A]],
          aa: Arbitrary[A]): Properties =
        newProperties("foldAlg") { p =>
          p.property("getGet") = getGet[P, A]
          p.property("getPut") = getPut[P, A]
          p.property("putGet") = putGet[P, A]
          p.property("putPut") = putPut[P, A]
        }
    }
  }

  /* natural representation properties */

  object nat {
    import core.nat._

    object foldAlg {

      def laws[P[_], A](implicit
          alg: FoldAlg[P, A],
          eq: Equal[P[(List[A], List[A])]]): Properties =
        newProperties("foldAlg") { p =>
          p.include(raw.foldAlg.laws)
        }
    }

    object lensAlg {

      def hom1[P[_], A](implicit
          alg: LensAlg[P, A],
          eq: Equal[P[A]],
          aa: Arbitrary[A]) =
        forAll(a => alg.natLensAlgLaw.hom1[A](a))

      def hom2[P[_], Q[_], A, X, Y](implicit
          alg: LensAlg.Aux[P, Q, A],
          eq: Equal[P[Y]],
          aqx: Arbitrary[Q[X]],
          axqy: Arbitrary[X => Q[Y]]) =
        forAll { (qx: Q[X], xqy: X => Q[Y]) =>
          alg.natLensAlgLaw.hom2[X, Y](qx)(xqy)
        }

      def laws[P[_], Q[_], A](implicit
          alg: LensAlg.Aux[P, Q, A],
          eq0: Equal[P[(A, A)]],
          eq1: Equal[P[Unit]],
          eq2: Equal[P[A]],
          eq3: Equal[P[Int]],
          aa: Arbitrary[A],
          aqi: Arbitrary[Q[Int]],
          aiqi: Arbitrary[Int => Q[Int]]): Properties =
        newProperties("lensAlg") { p =>
          p.include(raw.lensAlg.laws)
          p.property("hom1") = hom1[P, A]
          p.property("hom2") = hom2[P, Q, A, Int, Int]
        }
    }
  }
}
