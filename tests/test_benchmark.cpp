#include <tp/tpack.h>

namespace tp {

	constexpr auto big_int_list = generate<2000, int>();
	constexpr auto rev_big_int_list = reverse(big_int_list);

} // namespace tp