#include <tp/tpack.h>

namespace tp {

	constexpr auto big_int_list = generate<2000, int>();
	constexpr auto ints = filter(big_int_list, meta::value_adapter_v<std::is_same, int>);
	static_assert(size(big_int_list) == size(ints));

} // namespace tp