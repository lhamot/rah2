#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <utility>
#include <vector>
#include <type_traits>

namespace rah
{
    template <typename... Ts>
    using void_t = void;

    template <typename T, template <class> typename Lambda, typename Check = void>
    struct compiles_impl
    {
        static constexpr bool value = false;
    };

    template <typename T, template <class> class check_helper>
    struct compiles_impl<T, check_helper, void_t<check_helper<T>>>
    {
        static constexpr bool value = true;
    };

    template <typename T, template <class> class check_helper>
    static constexpr bool compiles = compiles_impl<T, check_helper>::value;

    template <typename T, typename U, template <class, class> typename Lambda, typename Check = void>
    struct compiles2_impl
    {
        static constexpr bool value = false;
    };

    template <typename T, typename U, template <class, class> class check_helper>
    struct compiles2_impl<T, U, check_helper, void_t<check_helper<T, U>>>
    {
        static constexpr bool value = true;
    };

    template <typename T, typename U, template <class, class> class check_helper>
    static constexpr bool compiles2 = compiles2_impl<T, U, check_helper>::value;

    template <typename R>
    struct test_impl
    {
        template <class T>
        using has_begin = decltype(begin(std::declval<T>()));

        template <class T>
        using has_iter_incr = std::enable_if_t<std::is_same_v<
            std::remove_reference_t<decltype(++begin(std::declval<T>()))>,
            std::remove_reference_t<decltype(begin(std::declval<T>()))>>>;

        template <class T>
        using has_end = decltype(begin(std::declval<T>()));

        static constexpr bool value =
            compiles<R, has_begin> && compiles<R, has_end> && compiles<R, has_iter_incr>;
    };

    template <typename T>
    constexpr bool test = test_impl<T>::value;

} // namespace rah
