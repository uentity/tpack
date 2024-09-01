#include <tp/tpack.h>

#include <tuple>

namespace tp {

	struct t0 {};
	struct t1 {};
	struct t2 {};
	struct t3 {};
	using t4 = std::tuple<t0,t1>;
	struct t5 {
		static constexpr int value = 1;
	};
	struct d0 : t0 {};

	// operator ==, !=
	static_assert(nil_v == nil_v);
	static_assert(unit_v<int> != nil_v);
	static_assert(unit<int>{} == unit<int>{});
	static_assert(unit<char>{} != unit<int>{});
	static_assert(unit_v<t0> == tpack_v<t0>);
	static_assert(tpack_v<t0, t1> == tpack_v<t0, t1>);
	static_assert(tpack_v<t0, t1> != tpack_v<t1, t0>);
	static_assert(tpack_v<t0&, t1> != tpack_v<t0, t1>);

	// operator +, concat
	static_assert(tpack_v<int> + tpack_v<char, bool> == tpack_v<int, char, bool>);
	static_assert(concat(tpack_v<int>, tpack_v<char, bool>) == tpack_v<int, char, bool>);
	static_assert(concat(tpack_v<int>, tpack_v<char, bool>, tpack_v<int&, double&>) == tpack_v<int, char, bool, int&, double&>);
	static_assert(concat(nil_v, tpack_v<char, bool>, tpack_v<int&, double&>) == tpack_v<char, bool, int&, double&>);

	// is_tpack
	static_assert(is_tpack_v<nil_tpack>);
	static_assert(is_tpack_v<const unit<int>&>);
	static_assert(is_tpack_v<tpack<int, char, double>&&>);
	static_assert(!is_tpack_v<int>);

	// apply
	static_assert(apply<is_tpack>(tpack_v<nil_tpack>) == unit_v<std::true_type>);
	static_assert(apply<is_tpack>(tpack_v<tpack<int, bool>>) == unit_v<std::true_type>);
	static_assert(apply<is_tpack>(tpack_v<int>) == unit_v<std::false_type>);

	// apply_v
	static_assert(apply_v<is_tpack>(tpack_v<nil_tpack>) == true);
	static_assert(apply_v<is_tpack>(tpack_v<tpack<int, bool>>) == true);
	static_assert(apply_v<is_tpack>(tpack_v<int>) == false);

	// make
	static_assert(std::is_same_v<make<tpack, tpack<int, char>>, tpack<int, char>>);

	// basic API: head, tail, push, pop
	static_assert(head(nil_v) == nil_v);
	static_assert(head(unit_v<int>) == unit_v<int>);
	static_assert(head(tpack_v<const int&&, char, double>) == unit_v<const int&&>);
	static_assert(tail(nil_v) == nil_v);
	static_assert(tail(unit_v<int>) == nil_v);
	static_assert(tail(tpack_v<const int&&, char, double>) == tpack_v<char, double>);
	static_assert(push_front<int>(tpack<double, char>{}) == tpack<int, double, char>{});
	static_assert(pop_front(tpack<int, double, char>{}) == tpack<double, char>{});
	static_assert(push_back<int>(tpack<double, char>{}) == tpack<double, char, int>{});
	static_assert(pop_back(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&>);

	// first
	static_assert(first<4>(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&, char>);
	static_assert(first<3>(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&, char>);
	static_assert(first<2>(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&>);
	static_assert(first<1>(tpack_v<int&, double&&, char>) == tpack_v<int&>);
	static_assert(first<0>(tpack_v<int&, double&&, char>) == nil_v);

	// last
	static_assert(last<4>(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&, char>);
	static_assert(last<3>(tpack_v<int&, double&&, char>) == tpack_v<int&, double&&, char>);
	static_assert(last<2>(tpack_v<int&, double&&, char>) == tpack_v<double&&, char>);
	static_assert(last<1>(tpack_v<int&, double&&, char>) == tpack_v<char>);
	static_assert(last<0>(tpack_v<int&, double&&, char>) == nil_v);

	// unwrap
	static_assert(strip(nil_v) == nil_v);
	static_assert(strip(unit_v<int>) == unit_v<int>);
	static_assert(strip(tpack_v<int, double, char>) == tpack_v<int, double, char>);
	static_assert(strip(unit_v<nil_tpack>) == nil_v);
	static_assert(strip(unit_v<unit<int>>) == unit_v<int>);
	static_assert(strip(unit_v<tpack<int, char, double>>) == tpack_v<int, char, double>);
	static_assert(strip(unit_v<unit<unit<tpack<int, char, double>>>>) == tpack_v<int, char, double>);

	// get
	static_assert(get<1>(tpack_v<double, int&, char>) == unit_v<int&>);

	// back
	static_assert(back(tpack_v<int, float, char*>) == unit_v<char*>);
	static_assert(back(nil_v) == nil_v);

	// contains
	static_assert(contains<int>(tpack<double, char, int>{}));
	static_assert(!contains<float>(tpack<double, char, int>{}));
	static_assert(!contains(nil_v, tpack<int>{}));

	// reverse
	static_assert(reverse(tpack_v<int&, double&&, char**>) == tpack_v<char**, double&&, int&>);

	// find
	static_assert(find<int>(tpack_v<int, double, char>) == 0);
	static_assert(find(tpack_v<int, double, char>, unit_v<double>) == 1);
	static_assert(find<char>(tpack_v<int, double, char>) == 2);
	static_assert(find(tpack_v<int, double, char>, unit_v<bool>) == 3);

	static_assert(find<0, 3, int>(tpack_v<int, double, int>) == 0);
	static_assert(find<1, 3, int>(tpack_v<int, double, int>) == 2);
	static_assert(find<1, 4>(tpack_v<int, double, int, int&>, unit_v<int>) == 2);
	static_assert(find<1, 4>(tpack_v<int, double, int, int, int&>, unit_v<int>) == 2);
	static_assert(find<0, 0, int>(tpack_v<int, double, int, int, int&>) == 5);
	static_assert(find<0, 1, int>(tpack_v<int, double, int, int, int&>) == 0);
	static_assert(find<1, 1, int>(tpack_v<int, double, int, int, int&>) == 5);
	static_assert(find<1, 3, int>(tpack_v<int, double, int, int, int&>) == 2);
	static_assert(find<2, 3, int>(tpack_v<int, double, int, int, int&>) == 2);
	static_assert(find<3, 5, int>(tpack_v<int, double, int, int, int&>) == 3);
	static_assert(find<10, 5, int>(tpack_v<int, double, int, int, int&>) == 5);

	// find_if
	static_assert(find_if<std::is_pointer>(tpack<int, int*, char>{}) == 1);

	// all_of, any_of, none_of, 
	static_assert(all_of<std::is_pointer>(tpack_v<char*, bool*>));
	static_assert(any_of<std::is_pointer>(tpack_v<char, bool*>));
	static_assert(!any_of<std::is_reference>(nil_v));

	// transform
	static_assert(transform<std::add_pointer>(tpack_v<int, double, char>) == tpack_v<int*, double*, char*>);
	static_assert(transform(tpack_v<int, double, char>, meta::type_adapter_v<std::add_pointer>) == tpack_v<int*, double*, char*>);

	// generate
	static_assert(generate<0, int>() == nil_v);
	static_assert(generate<3, int&>() == tpack_v<int&, int&, int&>);

	template<size_t I>
	using test_gen_type = unit<std::conditional_t<I%2 == 0, int, char>>;

	static_assert(generate<0, test_gen_type>() == nil_v);
	static_assert(generate<1, test_gen_type>() == tp::unit_v<int>);
	static_assert(generate<2, test_gen_type>() == tp::tpack_v<int, char>);
	static_assert(generate<4, test_gen_type>() == tp::tpack_v<int, char, int, char>);

	// filter
	static_assert(filter<std::is_pointer>(tpack_v<int*, int, char*>) == tpack_v<int*, char*>);
	static_assert(filter<std::is_pointer>(nil_v) == nil_v);

	// distinct
	static_assert(distinct(nil_v) == nil_v);
	static_assert(distinct(tpack_v<int>) == unit_v<int>);
	static_assert(distinct(tpack_v<int, int>) == unit_v<int>);
	static_assert(distinct(tpack_v<int, int, int>) == unit_v<int>);
	static_assert(distinct(tpack_v<char, int, char*, char, char, int, int&&>) == tpack_v<char, int, char*, int&&>);

	// fold_left, fold_right
	template<typename L, typename R>
	struct fold_always_lhs {
		using type = L;
	};

	static_assert(fold_left<fold_always_lhs>(nil_v) == nil_v);
	static_assert(fold_left<fold_always_lhs>(tpack_v<int>) == unit_v<int>);
	static_assert(fold_left<fold_always_lhs>(tpack_v<int>, unit_v<void>) == unit_v<void>);
	static_assert(fold_left<fold_always_lhs>(tpack_v<int, char, double>) == unit_v<int>);
	static_assert(fold_right<fold_always_lhs>(tpack_v<int, char, double>) == unit_v<int>);
	static_assert(fold_right<fold_always_lhs>(tpack_v<int, char, double>, unit_v<void>) == unit_v<int>);

	// simulate reverse with fold
	template<typename L, typename R>
	struct fold_reverse {
		using type = tpack<R, L>;
	};

	template<typename... Ts, typename R>
	struct fold_reverse<tpack<Ts...>, R> {
		using type = tpack<R, Ts...>;
	};

	template<typename L, typename... Ts>
	struct fold_reverse<L, tpack<Ts...>> {
		using type = tpack<Ts..., L>;
	};

	static_assert(fold_left<fold_reverse>(nil_v) == nil_v);
	static_assert(fold_left<fold_reverse>(unit_v<t0>) == unit_v<t0>);
	static_assert(fold_left<fold_reverse>(tpack_v<int, char, double>) == unit_v<tpack<double, char, int>>);
	static_assert(fold_left<fold_reverse>(tpack_v<int, char, double>, unit_v<void>) == unit_v<tpack<double, char, int, void>>);
	static_assert(fold_left<fold_reverse>(tpack_v<int, char, double>, unit_v<tpack<t0, t1>>) == unit_v<tpack<double, char, int, t0, t1>>);
	static_assert(fold_right<fold_reverse>(tpack_v<int, char, double>) == unit_v<tpack<double, char, int>>);
	static_assert(fold_right<fold_reverse>(tpack_v<int, char, double>, unit_v<void>) == unit_v<tpack<void, double, char, int>>);

} // namespace tp
