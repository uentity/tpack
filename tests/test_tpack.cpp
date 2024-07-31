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
	static_assert(find<int>(tpack<int, double, char>{}) == 0);
	static_assert(find(tpack<int, double, char>{}, tpack<double>{}) == 1);
	static_assert(find<char>(tpack<int, double, char>{}) == 2);
	static_assert(find(tpack<int, double, char>{}, unit<bool>{}) == 3);

	// find_if
	static_assert(find_if<std::is_pointer>(tpack<int, int*, char>{}) == 1);

	// all_of, any_of, none_of, 
	static_assert(all_of<std::is_pointer>(tpack_v<char*, bool*>));
	static_assert(any_of<std::is_pointer>(tpack_v<char, bool*>));
	static_assert(!any_of<std::is_reference>(nil_v));

	// transform
	static_assert(transform<std::add_pointer>(tpack_v<int, double, char>) == tpack_v<int*, double*, char*>);
	static_assert(transform(tpack_v<int, double, char>, mfn_type_adapter<std::add_pointer>) == tpack_v<int*, double*, char*>);

	// generate
	static_assert(generate<3, int&>() == tpack_v<int&, int&, int&>);

	// filter
	static_assert(filter<std::is_pointer>(tpack_v<int*, int, char*>) == tpack_v<int*, char*>);
	static_assert(filter<std::is_pointer>(nil_v) == nil_v);

	// distinct
	static_assert(distinct(nil_v) == nil_v);
	static_assert(distinct(tpack_v<int>) == unit_v<int>);
	static_assert(distinct(tpack_v<int, int>) == unit_v<int>);
	static_assert(distinct(tpack_v<char, int, char*,  char, char, int, int&&>) == tpack_v<char, int, char*, int&&>);

	// fold_left, fold_right
	template<typename L, typename R>
	struct fold_always_lhs {
		static constexpr auto value = unit_v<L>;
	};

	static_assert(fold_left<fold_always_lhs>(nil_v) == nil_v);
	static_assert(fold_left<fold_always_lhs>(tpack_v<int>) == unit_v<int>);
	static_assert(fold_left<fold_always_lhs>(tpack_v<int>, unit_v<void>) == unit_v<void>);
	static_assert(fold_left<fold_always_lhs>(tpack_v<int, char, double>) == unit_v<int>);

	static_assert(fold_right<fold_always_lhs>(tpack_v<int, char, double>) == unit_v<double>);
	static_assert(fold_right<fold_always_lhs>(tpack_v<int, char, double>, unit_v<void>) == unit_v<void>);

	// simulate reverse with fold
	template<typename L, typename R>
	struct fold_reverse {
		static constexpr auto value = unit_v<tpack<R, L>>;
	};

	template<typename... Ts, typename R>
	struct fold_reverse<tpack<Ts...>, R> {
		static constexpr auto value = unit_v<tpack<R, Ts...>>;
	};

	static_assert(fold_left<fold_reverse>(nil_v) == nil_v);
	static_assert(fold_left<fold_reverse>(unit_v<t0>) == unit_v<t0>);
	static_assert(fold_left<fold_reverse>(tpack_v<int, char, double>) == unit_v<tpack<double, char, int>>);
	static_assert(fold_left<fold_reverse>(tpack_v<int, char, double>, unit_v<void>) == unit_v<tpack<double, char, int, void>>);
	static_assert(fold_left<fold_reverse>(tpack_v<int, char, double>, unit_v<tpack<t0, t1>>) == unit_v<tpack<double, char, int, t0, t1>>);

	static_assert(fold_right<fold_reverse>(tpack_v<int, char, double>) == unit_v<tpack<int, char, double>>);
	static_assert(fold_right<fold_reverse>(tpack_v<int, char, double>, unit_v<void>) == unit_v<tpack<int, char, double, void>>);

} // namespace tp
