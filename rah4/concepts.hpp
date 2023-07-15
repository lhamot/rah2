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

    template <bool Diagnostic>
    struct compiles_impl;

    template <>
    struct compiles_impl<false>
    {
        template <typename T, template <class> class Lambda, typename Check = void>
        struct compiles
        {
            static constexpr bool value = false;
        };

        template <typename T, template <class> class check_helper>
        struct compiles<T, check_helper, void_t<check_helper<T>>>
        {
            static constexpr bool value = true;
        };
    };

    template <>
    struct compiles_impl<true>
    {
        template <typename T, template <class> class check_helper>
        struct compiles
        {
            using type = check_helper<T>;
            static constexpr bool value = true;
        };
    };

    template <bool Diagnostic, typename T, template <class> class check_helper>
    static constexpr bool compiles =
        compiles_impl<Diagnostic>::template compiles<T, check_helper>::value;

    template <bool Diagnostic, typename T, typename U, template <class, class> class Lambda, typename Check = void>
    struct compiles2_impl
    {
        static_assert(!Diagnostic, "!Diagnostic");
        static constexpr bool value = false;
    };

    template <bool Diagnostic, typename T, typename U, template <class, class> class check_helper>
    struct compiles2_impl<Diagnostic, T, U, check_helper, void_t<check_helper<T, U>>>
    {
        static constexpr bool value = true;
    };

    template <bool Diagnostic, typename T, typename U, template <class, class> class check_helper>
    static constexpr bool compiles2 = compiles2_impl<Diagnostic, T, U, check_helper>::value;

    template <typename R>
    struct test_impl
    {
        template <class T>
        using has_begin = decltype(begin(std::declval<T>()));

        template <class T>
        using has_iter_incr = std::enable_if_t<std::is_same<
            std::remove_reference_t<decltype(++begin(std::declval<T>()))>,
            std::remove_reference_t<decltype(begin(std::declval<T>()))>>::value>;

        template <class T>
        using has_end = decltype(begin(std::declval<T>()));

        static constexpr bool value = compiles<false, R, has_begin> && compiles<false, R, has_end>
                                      && compiles<false, R, has_iter_incr>;
    };

    template <typename T>
    constexpr bool test = test_impl<T>::value;

    namespace concepts
    {
        template <typename...>
        struct TypeList
        {
        };

        template <bool Diagnostic, bool Val>
        struct is_true_impl;

    } // namespace concepts

    template <bool Diagnostic, bool Val>
    struct is_true_impl;

    template <>
    struct is_true_impl<true, false>
    {
        // static_assert(false, "Requirement not meet");
        // using type = int;
    };

    template <>
    struct is_true_impl<true, true>
    {
        // static_assert(Val, "Requirement not meet");
        using type = int;
    };

    template <>
    struct is_true_impl<false, true>
    {
        using type = int;
    };

    template <bool Diagnostic, bool Val>
    using is_true = typename is_true_impl<Diagnostic, Val>::type;

    template <bool Diagnostic, bool Val>
    struct is_true_v_impl;

    template <>
    struct is_true_v_impl<true, false>;

    template <>
    struct is_true_v_impl<true, true>
    {
        static constexpr bool value = true;
    };

    template <>
    struct is_true_v_impl<false, true>
    {
        static constexpr bool value = true;
    };

    template <>
    struct is_true_v_impl<false, false>
    {
        static constexpr bool value = false;
    };

    template <bool Diagnostic, bool Val>
    static constexpr bool is_true_v = is_true_v_impl<Diagnostic, Val>::value;

} // namespace rah
