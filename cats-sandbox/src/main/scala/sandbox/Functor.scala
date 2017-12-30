// trait Functor[F[_]] {
//   def map[A, B](fa: F[A])(f: A => B): F[B]
// }

// object FunctorLaws {
//   def identityLaw[F[_], A](fa: F[A])(implicit ev: Functor[F]): Boolean = {
//     ev.map(fa)((a: A) => a) == ev
//   }

//   def compositionLaw[F[_], A, B, C](fa: F[A])(f: A => B)(g: B => C)(implicit ev: Functor[F]): Boolean = {
//     ev.map(fa)((a) => g(f(a))) == ev.map(ev.map(fa)(f))(g)
//   }
// }
