#include <tp/tpack.h>

namespace tp {

	constexpr auto big_int_list = generate<2000, int>();
	constexpr auto rev_big_int_list = reverse(big_int_list);
	constexpr auto one_int = distinct(big_int_list);
	static_assert(one_int == unit_v<int>);

	constexpr auto l1 = push_back(big_int_list, unit_v<char>);
	static_assert(contains(l1, unit_v<char>));
	static_assert(find<char>(l1) == 2000);

	template<size_t I>
	using test_gen_type = unit<std::conditional_t<I%2 == 0, int, char>>;

	constexpr auto big_ic_list = generate<1000, test_gen_type>();
	constexpr auto ic_pair = distinct(big_ic_list);
	static_assert(ic_pair == tpack_v<int, char>);

} // namespace tp