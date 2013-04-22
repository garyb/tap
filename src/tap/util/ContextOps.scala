package tap.util

import language.implicitConversions

object ContextOps {

	implicit def withContext[C](ctx: C) = new {

		def map[A, B](xs: List[A])(fn: (C, A) => (C, B)): (C, List[B]) = {
			xs.foldRight((ctx, List.empty[B])) { case (x, (ctx, xs)) =>
				val (ctx1, x1) = fn(ctx, x)
				(ctx1, x1 :: xs)
			}
		}

		def map[A, K, V, K1, V1](xs: Map[K, V])(fn: (C, (K, V)) => (C, (K1, V1))): (C, Map[K1, V1]) = {
			xs.foldRight((ctx, Map.empty[K1, V1])) { case (kv, (ctx, xs)) =>
				val (ctx1, kv1) = fn(ctx, kv)
				(ctx1, xs + kv1)
			}
		}

		def flatMap[A, B](xs: List[A])(fn: (C, A) => (C, List[B])): (C, List[B]) = {
			xs.foldRight((ctx, List.empty[B])) { case (x, (ctx, xs)) =>
				val (ctx1, ys) = fn(ctx, x)
				(ctx1, ys ++ xs)
			}
		}

		def foreach[A](xs: List[A])(fn: (C, A) => C): C = {
			xs.foldRight(ctx) { case (x, ctx) => fn(ctx, x) }
		}
	}
}
