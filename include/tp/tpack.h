#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

namespace tp {

	template<typename... Ts>
	struct tpack {};

	template<typename T>
	struct tpack<T> {
		using type = T;
	};

	template<typename... Ts>
	inline constexpr auto tpack_v = tpack<Ts...>{};

	// `unit` is an alias to `tpack<T>` with single element
	template<typename T>
	using unit = tpack<T>;

	template<typename T>
	inline constexpr auto unit_v = unit<T>{};

	// `nil` is empty tpack
	using nil_tpack = tpack<>;
	inline constexpr auto nil_v = nil_tpack{};
	// [NOTE] `tpack_v<nil_tpack>` is not the same as `nil_v`:
	// tpack<tpack<>> != tpack<>

	// operators
	template<typename... Ts, typename... Us>
	constexpr bool operator==(tpack<Ts...>, tpack<Us...>) { return false; }

	template<typename... Ts>
	constexpr bool operator==(tpack<Ts...>, tpack<Ts...>) { return true; }

	template<typename... Ts, typename... Us>
	constexpr bool operator!=(tpack<Ts...> x, tpack<Us...> y) { return !(x == y); }

	template<typename... Ts, typename... Us>
	constexpr auto operator+(tpack<Ts...>, tpack<Us...>) { return tpack_v<Ts..., Us...>; }

	// test if type is tpack
	template<typename T>
	struct is_tpack : std::false_type {};

	template<typename... Ts>
	struct is_tpack<tpack<Ts...>> : std::true_type {};

	template<typename T>
	using is_tpack_t = typename is_tpack<std::remove_cv_t<std::remove_reference_t<T>>>::type;

	template<typename T>
	inline constexpr bool is_tpack_v = is_tpack<std::remove_cv_t<std::remove_reference_t<T>>>::value;

	// helper to suppress unused expression result compiler warnings
	struct ignore_t {
		constexpr ignore_t() {}

		template<typename T>
		constexpr auto& operator=(T&&) const { return *this; }
	};

	inline constexpr auto ignore = ignore_t{};

	// adapters for meta functions -> constexpr functions taking type packs (typically `unit`)
	template<template<typename...> typename F, typename... Ts>
	struct mfn_value {
		template<typename... Us>
		static constexpr auto call(tpack<Us...>) {
			return F<Ts..., Us...>::value;
		}

		template<typename... Us>
		constexpr auto operator()(tpack<Us...> x) const { return call(x); }
	};

	template<template<typename...> typename F, typename... Ts>
	inline constexpr auto mfn_value_adapter = mfn_value<F, Ts...>{};

	template<template<typename...> typename F, typename... Ts>
	struct mfn_type {
		template<typename... Us>
		static constexpr auto call(tpack<Us...>) {
			return unit_v<typename F<Ts..., Us...>::type>;
		}

		template<typename... Us>
		constexpr auto operator()(tpack<Us...> x) const { return call(x); }
	};

	template<template<typename...> typename F, typename... Ts>
	inline constexpr auto mfn_type_adapter = mfn_type<F, Ts...>{};

	template<template<typename...> typename F, typename... Ts>
	struct mfn_apply {
		template<typename... Us>
		static constexpr auto call(tpack<Us...>) {
			return unit_v<F<Ts..., Us...>>;
		}

		template<typename... Us>
		constexpr auto operator()(tpack<Us...> x) const { return call(x); }
	};

	template<typename F, typename... Args>
	using fn_result_t = typename decltype(std::declval<F>()(std::declval<Args>()...))::type;

	// apply - only makes sense for metafunctions
	template<template<typename...> typename F, typename... Ts>
	constexpr auto apply(tpack<Ts...> tp) {
		return mfn_type<F>::call(tp);
	}

	template<template<typename... Ts> typename F, typename... Us>
	constexpr auto apply_v(tpack<Us...> tp) {
		return mfn_value<F>::call(tp);
	}

	template<template<typename...> typename F, typename TP>
	constexpr auto make_v(TP tp) {
		return mfn_apply<F>::call(tp);
	}

	template<template<typename...> typename F, typename TP>
	using make = typename decltype(make_v<F>(std::declval<TP>()))::type;

	// basic API
	template<typename... Ts>
	constexpr auto size(tpack<Ts...>) { return sizeof...(Ts); }

	template<typename... Ts>
	constexpr bool empty(tpack<Ts...> tp) { return size(tp) == 0; }

	template<typename T, typename... Ts>
	constexpr auto head(tpack<T, Ts...>) -> unit<T> { return {}; }

	constexpr auto head(nil_tpack) -> nil_tpack { return {}; }

	template<typename... Ts>
	using head_t = typename decltype(head(tpack_v<Ts...>))::type;

	template<typename T, typename... Ts>
	constexpr auto tail(tpack<T, Ts...>) -> tpack<Ts...> { return {}; }

	constexpr auto tail(nil_tpack) -> nil_tpack { return {}; }

	template<typename... TPs>
	constexpr auto concat(TPs... tps) {
		return (tps + ...);
	}

	// unwrap: strips `unit<unit<...tpack<Ts...>...>>` -> tpack<Ts...>
	template<typename T, typename... Ts>
	constexpr auto strip(tp::tpack<T, Ts...> tp) {
		if constexpr (sizeof...(Ts) == 0 && is_tpack_v<T>)
			return strip(T{});
		else
			return tp;
	}

	constexpr auto strip(nil_tpack) -> nil_tpack { return {}; }

	// push front/back
	template<typename T, typename... Ts>
	constexpr auto push_front(tpack<Ts...>) -> tpack<T, Ts...> { return {}; }

	template<typename... Ts, typename T>
	constexpr auto push_front(tpack<Ts...>, unit<T>) -> tpack<T, Ts...> { return {}; }

	template<typename T, typename... Ts>
	constexpr auto pop_front(tpack<T, Ts...>) -> tpack<Ts...> { return {}; }

	template<typename T, typename... Ts>
	constexpr auto push_back(tpack<Ts...>) -> tpack<Ts..., T> { return {}; }

	template<typename... Ts, typename T>
	constexpr auto push_back(tpack<Ts...>, unit<T>) -> tpack<Ts..., T> { return {}; }

	// get
	namespace detail {

		struct unit_placeholder {
			template<typename T>
			unit_placeholder(unit<T>);
		};

		template<typename T>
		struct get_impl;

		template<size_t... Is>
		struct get_impl<std::index_sequence<Is...>> {
			// `unit<T>` at `Is` position used to infer type `T` of the argument placed there
			// which is recorded as return type
			template<typename T>
			static constexpr T get(decltype(Is, std::declval<unit_placeholder>())..., unit<T>, ...);
		};

	} // namespace detail

	template<std::size_t I, typename... Ts>
	constexpr auto get(tpack<Ts...>) {
		return unit_v<decltype( detail::get_impl<std::make_index_sequence<I>>::get(unit_v<Ts>...) )>;
	}

	// back
	template<typename... Ts>
	constexpr auto back(tpack<Ts...> tp) {
		if constexpr(empty(tp))
			return nil_v;
		else
			return get<size(tp) - 1>(tp);
	}

	template<typename... Ts>
	using back_t = typename decltype(back(tpack_v<Ts...>))::type;

	// pop_back
	namespace detail {

		template<std::size_t... Is, typename TP>
		constexpr auto get_first(std::index_sequence<Is...>, TP tp) {
			return tpack_v<typename decltype(get<Is>(tp))::type...>;
		}

	} // namespace detail

	template<typename... Ts>
	constexpr auto pop_back(tpack<Ts...> tp) {
		if constexpr(empty(tp))
			return tp;
		else
			return detail::get_first(std::make_index_sequence<size(tp) - 1>{}, tp);
	}

	// reverse
	namespace detail {

		template<std::size_t... Is, typename TP>
		constexpr auto do_reverse(std::index_sequence<Is...>, TP tp) {
			return tpack<typename decltype(get<size(tp) - Is - 1>(tp))::type...>{};
		}

	} // namespace detail

	template<typename... Ts>
	constexpr auto reverse(tpack<Ts...> tp) {
		return detail::do_reverse(std::index_sequence_for<Ts...>{}, tp);
	}

	// contains
	template<typename T, typename... Ts>
	constexpr bool contains(tpack<Ts...>) {
		return (std::is_same_v<T, Ts> || ...);
	}

	template<typename... Ts, typename T>
	constexpr bool contains(tpack<Ts...> x, unit<T>) { return contains<T>(x); }

	// find
	template<typename T, typename... Ts>
	constexpr auto find(tpack<Ts...>) -> std::size_t {
		std::size_t res = 0;
		ignore = ( (std::is_same_v<T, Ts> ? true : (++res, false)) || ... );
		return res;
	}

	template<typename... Ts, typename T>
	constexpr auto find(tpack<Ts...> x, unit<T>) { return find<T>(x); }

	// find_if
	template<typename... Ts, typename Pred>
	constexpr auto find_if(tpack<Ts...>, Pred f) -> std::size_t {
		std::size_t res = 0;
		ignore = ( (f(unit_v<Ts>) ? true : (++res, false)) || ... );
		return res;
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto find_if(tpack<Ts...> x) {
		return find_if(x, mfn_value_adapter<Pred>);
	}

	// all_of
	template<typename... Ts, typename Pred>
	constexpr bool all_of(tpack<Ts...>, Pred f) {
		return (f(unit_v<Ts>) && ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto all_of(tpack<Ts...> x) {
		return all_of(x, mfn_value_adapter<Pred>);
	}

	// any_of
	template<typename... Ts, typename Pred>
	constexpr bool any_of(tpack<Ts...>, Pred f) {
		return (f(unit_v<Ts>) || ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto any_of(tpack<Ts...> x) {
		return any_of(x, mfn_value_adapter<Pred>);
	}

	// none_of
	template<typename... Ts, typename Pred>
	constexpr bool none_of(tpack<Ts...> x, Pred f) {
		return !any_of(x, f);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto none_of(tpack<Ts...> x) {
		return none_of(x, mfn_value_adapter<Pred>);
	}

	// transform
	template<typename... Ts, typename F>
	constexpr auto transform(tpack<Ts...> x, F f) {
		return tpack_v<fn_result_t<F, unit<Ts>>...>;
	}

	template<template<typename...> typename F, typename... Ts>
	constexpr auto transform(tpack<Ts...> x) {
		return tpack_v<typename F<Ts>::type...>;
	}

	// generate
	namespace detail {

		template<typename T, std::size_t... Is>
		constexpr auto do_generate(std::index_sequence<Is...>) {
			return (((void)Is, unit_v<T>) + ...);
		}

	} // namespace detail

	template<std::size_t N, typename T>
	constexpr auto generate() {
		return detail::do_generate<T>(std::make_index_sequence<N>{});
	}

	// filter
	namespace detail {
		
		template<typename T, typename F>
		constexpr auto filter_one(unit<T> t, F f) {
			if constexpr(f(t))
				return t;
			else
				return nil_v;
		}

	} // namespace detail

	template<typename... Ts, typename Pred>
	constexpr auto filter(tpack<Ts...>, Pred f) {
		return (detail::filter_one(unit_v<Ts>, f) + ... + nil_v);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto filter(tpack<Ts...> tp) {
		return filter(tp, mfn_value_adapter<Pred>);
	}

	// distinct
	namespace detail {

		template<typename... Ts, typename... Rs>
		constexpr auto do_distinct(tpack<Ts...> src, tpack<Rs...> dst) {
			if constexpr (empty(src))
				return dst;
			else {
				const auto first = head(src);
				if constexpr (find(dst, first) < size(dst))
					return do_distinct(pop_front(src), dst);
				else
				 	return do_distinct(pop_front(src), push_back(dst, first));
			}
		}

	} // namespace detail

	template<typename... Ts>
	constexpr auto distinct(tp::tpack<Ts...> tp) {
		return detail::do_distinct(tp, nil_v);
	}

	// fold_left, fold_right
	namespace detail {

		template<typename T, typename R, typename F>
		constexpr auto do_fold_left(T, R, F) {
			static_assert(false, "Folding meta-function `F` must produce `F::value` of type `unit<T>`");
		}

		template<typename... Ts, typename... Rs, typename F>
		constexpr auto do_fold_left(tpack<Ts...> tp, tpack<Rs...> res, F f) {
			if constexpr (empty(tp))
				return res;
			else
				return do_fold_left(pop_front(tp), f(res + head(tp)), f);
		}

	} // namespace detail

	template<typename... Ts, typename F, typename D = nil_tpack>
	constexpr auto fold_left(tpack<Ts...> tp, F f, D seed = nil_v) {
		if constexpr (empty(seed) && size(tp) > 0)
			return detail::do_fold_left(pop_front(tp), head(tp), f);
		else
			return detail::do_fold_left(tp, seed, f);
	}

	// If `F` is a classic meta-function that calculates `F::type`,
	// use `fold_left(tp, mfn_type_adapter<F>, seed)` call
	template<template<typename L, typename R> typename F, typename... Ts, typename D = nil_tpack>
	constexpr auto fold_left(tpack<Ts...> tp, D seed = nil_v) {
		return fold_left(tp, mfn_value_adapter<F>, seed);
	}

	template<typename... Ts, typename F, typename D = nil_tpack>
	constexpr auto fold_right(tpack<Ts...> tp, F f, D seed = nil_v) {
		return fold_left(reverse(tp), f, seed);
	}

	// If `F` is a classic meta-function that calculates `F::type`,
	// use `fold_right(tp, mfn_type_adapter<F>, seed)` call
	template<template<typename L, typename R> typename F, typename... Ts, typename D = nil_tpack>
	constexpr auto fold_right(tpack<Ts...> tp, D seed = nil_v) {
		return fold_right(tp, mfn_value_adapter<F>, seed);
	}

} // namespace tp
