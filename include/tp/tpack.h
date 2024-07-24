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
	inline static constexpr auto tpack_v = tpack<Ts...>{};

	template<typename T>
	using unit = tpack<T>;

	template<typename T>
	inline static constexpr auto unit_v = unit<T>{};

	using nil_tpack = tpack<>;
	inline static constexpr auto nil_v = nil_tpack{};

	template<typename... Ts, typename... Us>
	constexpr bool operator==(tpack<Ts...>, tpack<Us...>) { return false; }

	template<typename... Ts>
	constexpr bool operator==(tpack<Ts...>, tpack<Ts...>) { return true; }

	template<typename... Ts, typename... Us>
	constexpr bool operator!=(tpack<Ts...> x, tpack<Us...> y) { return !(x == y); }

	template<typename... Ts, typename... Us>
	constexpr auto operator+(tpack<Ts...> x, tpack<Us...> y) { return tpack_v<Ts..., Us...>; }

	static_assert(unit<int>{} == unit<int>{});
	static_assert(unit<char>{} != unit<int>{});

	// test if type is tpack
	template<typename T>
	struct is_tpack : std::false_type {};

	template<typename... Ts>
	struct is_tpack<tpack<Ts...>> : std::true_type {};

	template<typename T>
	using is_tpack_t = typename is_tpack<std::remove_cv_t<std::remove_reference_t<T>>>::type;

	template<typename T>
	inline static constexpr bool is_tpack_v = is_tpack<std::remove_cv_t<std::remove_reference_t<T>>>::value;

	static_assert(is_tpack_v<nil_tpack>);
	static_assert(is_tpack_v<const unit<int>&>);
	static_assert(is_tpack_v<tpack<int, char, double>&&>);
	static_assert(!is_tpack_v<int>);

	struct ignore_t {
		template<typename... Ts>
		constexpr ignore_t(Ts&&...) {}

		template<typename T>
		constexpr auto& operator=(T&&) const { return *this; }
	};

	inline static constexpr auto ignore = ignore_t{};

	// meta functions adapters -> constexpr functions taking type packs (typically `unit`)

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
	inline static constexpr auto mfn_value_adapter = mfn_value<F, Ts...>{};

	template<template<typename...> typename F, typename... Ts>
	struct mfn_type {
		template<typename... Us>
		static constexpr auto call(tpack<Us...>) {
			return unit<typename F<Ts..., Us...>::type>{};
		}

		template<typename... Us>
		constexpr auto operator()(tpack<Us...> x) const { return call(x); }
	};

	template<template<typename...> typename F, typename... Ts>
	inline static constexpr auto mfn_type_adapter = mfn_type<F, Ts...>{};

	template<template<typename...> typename F, typename... Ts>
	struct mfn_apply {
		template<typename... Us>
		static constexpr auto call(tpack<Us...>) {
			return unit<F<Ts..., Us...>>{};
		}

		template<typename... Us>
		constexpr auto operator()(tpack<Us...> x) const { return call(x); }
	};

	template<typename F, typename... Args>
	using adapter_result_t = typename decltype(std::declval<F>()(std::declval<Args>()...))::type;

	// apply - only makes sense for metafunctions
	template<template<typename...> typename F, typename... Ts>
	constexpr auto apply(tpack<Ts...> tp) {
		return mfn_type<F>::call(tp);
	}

	template<template<typename...> typename F, typename TP>
	using make = typename decltype(mfn_apply<F>::call(std::declval<TP>()))::type;

	static_assert(apply<is_tpack>(tpack_v<nil_tpack>) == unit_v<std::true_type>);
	static_assert(apply<is_tpack>(tpack_v<tpack<int, bool>>) == unit_v<std::true_type>);
	static_assert(apply<is_tpack>(tpack_v<int>) == unit_v<std::false_type>);

	static_assert(std::is_same_v<make<tpack, tpack<int, char>>, tpack<int, char>>);

	template<template<typename... Ts> typename F, typename... Us>
	constexpr auto apply_v(tpack<Us...> tp) {
		return mfn_value<F>::call(tp);
	}

	static_assert(apply_v<is_tpack>(tpack_v<nil_tpack>) == true);
	static_assert(apply_v<is_tpack>(tpack_v<tpack<int, bool>>) == true);
	static_assert(apply_v<is_tpack>(tpack_v<int>) == false);

	// basic API

	template<typename... Ts>
	constexpr auto size(tpack<Ts...>) { return sizeof...(Ts); }

	template<typename... Ts>
	constexpr bool empty(tpack<Ts...> tp) { return size(tp) == 0; }

	template<typename T, typename... Ts>
	constexpr auto head(tpack<T, Ts...>) -> unit<T> { return {}; }

	template<typename T, typename... Ts>
	constexpr auto tail(tpack<T, Ts...>) -> tpack<Ts...> { return {}; }

	template<typename... TPs>
	constexpr auto concat(TPs... tps) {
		return (tps + ...);
	}

	static_assert(tpack_v<int> + tpack_v<char, bool> == tpack_v<int, char, bool>);
	static_assert(concat(tpack_v<int>, tpack_v<char, bool>) == tpack_v<int, char, bool>);
	static_assert(concat(tpack_v<int>, tpack_v<char, bool>, tpack_v<int&, double&>) == tpack_v<int, char, bool, int&, double&>);
	static_assert(concat(nil_v, tpack_v<char, bool>, tpack_v<int&, double&>) == tpack_v<char, bool, int&, double&>);

	// push front/back

	template<typename T, typename... Ts>
	constexpr auto push_front(tpack<Ts...>) -> tpack<T, Ts...> { return {}; }

	template<typename... Ts, typename T>
	constexpr auto push_front(tpack<Ts...>, unit<T>) -> tpack<T, Ts...> { return {}; }

	static_assert(push_front<int>(tpack<double, char>{}) == tpack<int, double, char>{});

	template<typename T, typename... Ts>
	constexpr auto pop_front(tpack<T, Ts...>) -> tpack<Ts...> { return {}; }

	static_assert(pop_front(tpack<int, double, char>{}) == tpack<double, char>{});

	template<typename T, typename... Ts>
	constexpr auto push_back(tpack<Ts...>) -> tpack<Ts..., T> { return {}; }

	template<typename... Ts, typename T>
	constexpr auto push_back(tpack<Ts...>, unit<T>) -> tpack<Ts..., T> { return {}; }

	static_assert(push_back<int>(tpack<double, char>{}) == tpack<double, char, int>{});

	// get
	namespace detail {

		struct unit_placeholder {
			template<typename T>
			unit_placeholder(unit<T>) {}
		};

		template<typename T>
		struct get_impl;

		template<size_t... Is>
		struct get_impl<std::index_sequence<Is...>> {
			template<typename T>
			static constexpr T get(decltype(Is, std::declval<unit_placeholder>())..., unit<T>, ...);
		};

	} // namespace detail

	template<std::size_t I, typename... Ts>
	constexpr auto get(tpack<Ts...>) {
		return unit_v<decltype( detail::get_impl<std::make_index_sequence<I>>::get(unit_v<Ts>...) )>;
	}

	static_assert(get<1>(tpack_v<double, int&, char>) == unit_v<int&>);

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

	static_assert(reverse(tpack_v<int&, double&&, char**>) == tpack_v<char**, double&&, int&>);

	// back

	template<typename... Ts>
	constexpr auto back(tpack<Ts...> tp) {
		if constexpr(empty(tp))
			return nil_v;
		else
			return get<size(tp) - 1>(tp);
	}

	static_assert(back(tpack_v<int, float, char*>) == unit_v<char*>);
	static_assert(back(nil_v) == nil_v);

	// checks
	template<typename T, typename... Ts>
	constexpr bool contains(tpack<Ts...>) {
		return (std::is_same_v<T, Ts> || ...);
	}

	template<typename... Ts, typename T>
	constexpr bool contains(tpack<Ts...> x, unit<T>) { return contains<T>(x); }

	static_assert(contains<int>(tpack<double, char, int>{}));
	static_assert(!contains<float>(tpack<double, char, int>{}));
	static_assert(!contains(nil_v, tpack<int>{}));

	// find
	template<typename T, typename... Ts>
	constexpr auto find(tpack<Ts...>) -> std::size_t {
		std::size_t res = 0;
		ignore = ( (std::is_same_v<T, Ts> ? true : (++res, false)) || ... );
		return res;
	}

	template<typename... Ts, typename T>
	constexpr auto find(tpack<Ts...> x, unit<T>) { return find<T>(x); }

	static_assert(find<int>(tpack<int, double, char>{}) == 0);
	static_assert(find(tpack<int, double, char>{}, tpack<double>{}) == 1);
	static_assert(find<char>(tpack<int, double, char>{}) == 2);
	static_assert(find(tpack<int, double, char>{}, unit<bool>{}) == 3);

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

	static_assert(find_if<std::is_pointer>(tpack<int, int*, char>{}) == 1);

	// all_of
	template<typename... Ts, typename Pred>
	constexpr bool all_of(tpack<Ts...>, Pred f) {
		return (f(unit_v<Ts>) && ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto all_of(tpack<Ts...> x) {
		return all_of(x, mfn_value_adapter<Pred>);
	}

	static_assert(all_of<std::is_pointer>(tpack_v<char*, bool*>));

	// any_of
	template<typename... Ts, typename Pred>
	constexpr bool any_of(tpack<Ts...>, Pred f) {
		return (f(unit_v<Ts>) || ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto any_of(tpack<Ts...> x) {
		return any_of(x, mfn_value_adapter<Pred>);
	}

	static_assert(any_of<std::is_pointer>(tpack_v<char, bool*>));
	static_assert(!any_of<std::is_reference>(nil_v));

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
		return tpack_v<adapter_result_t<F, unit<Ts>>...>;
	}

	template<template<typename...> typename F, typename... Ts>
	constexpr auto transform(tpack<Ts...> x) {
		return tpack_v<typename F<Ts>::type...>;
	}

	static_assert(transform<std::add_pointer>(tpack_v<int, double, char>) == tpack_v<int*, double*, char*>);
	static_assert(transform(tpack_v<int, double, char>, mfn_type_adapter<std::add_pointer>) == tpack_v<int*, double*, char*>);

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

	static_assert(pop_back(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&>);

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

	static_assert(generate<3, int&>() == tpack_v<int&, int&, int&>);

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

	static_assert(filter<std::is_pointer>(tpack_v<int*, int, char*>) == tpack_v<int*, char*>);
	static_assert(filter<std::is_pointer>(nil_v) == nil_v);

} // namespace typelist