#include <tp/tpack.h>

namespace tp {

	// simulate reverse with fold
	template<typename L, typename R>
	struct fold_reverse {
		using type = tpack<R, L>;
	};

	template<typename... Ts, typename R>
	struct fold_reverse<tpack<Ts...>, R> {
		using type = tpack<R, Ts...>;
	};

	// simulate distinct with fold
	template<typename T, typename U>
	struct fold_distinct {
		using type = tpack<T, U>;
	};

	template<typename T>
	struct fold_distinct<T, T> {
		using type = unit<T>;
	};

	template<typename T, typename... Ts>
	struct fold_distinct<tpack<Ts...>, T> {
		using type = std::conditional_t<
			find<T, Ts...>({}) == sizeof...(Ts),
			tpack<Ts..., T>, tpack<Ts...>
		>;
	};

	// unique types generators
	template<std::size_t I>
	struct gen_unique_type {
		struct type {};
	};

	template<std::size_t I>
	struct gen_unique_type1 {
		struct type {};
	};

	// generate int, char sequence
	template<size_t I>
	using gen_int_char = unit<std::conditional_t<I%2 == 0, int, char>>;

	// -------------------------------------
	constexpr auto big_int_list = generate<2000, int>();
	constexpr auto rev_big_int_list = reverse(big_int_list);
	constexpr auto one_int = distinct(big_int_list);
	static_assert(one_int == unit_v<int>);

	constexpr auto l1 = push_back(rev_big_int_list, unit_v<char>);
	static_assert(contains(l1, unit_v<char>));
	static_assert(find<char>(l1) == 2000);

	constexpr auto big_ic_list = generate<2000, gen_int_char>();
	constexpr auto ic_pair = distinct(big_ic_list);
	static_assert(ic_pair == tpack_v<int, char>);
	constexpr auto only_ints = filter(big_ic_list, meta::value_adapter_v<std::is_same, int>);
	static_assert(size(only_ints) == 1000 && !contains(only_ints, unit_v<char>));

	constexpr auto unique_ts = generate<500, gen_unique_type>();
	constexpr auto rev_unique_ts = reverse(unique_ts);
	constexpr auto unique_ts_rev = decltype(fold_left<fold_reverse>(unique_ts))::type{};
	static_assert(rev_unique_ts == unique_ts_rev);

	constexpr auto unique_ts1 = generate<500, gen_unique_type1>();
	constexpr auto unique_ts11 = distinct(unique_ts1);
	static_assert(unique_ts1 == unique_ts11);
	constexpr auto unique_ts12 = decltype(fold_left<fold_distinct>(unique_ts1))::type{};
	static_assert(unique_ts12 == unique_ts12);

} // namespace tp