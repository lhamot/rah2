#pragma once

#ifndef RAH2_STD
#define RAH2_STD ::std
#endif

#ifndef RAH2_NS
#define RAH2_NS rah2
#endif

namespace RAH2_NS
{
    namespace concepts
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
    } // namespace concepts
} // namespace RAH2_NS

#define RAH2_MAKE_CONCEPT(NAME, NEED_COMPILE)                                                      \
    namespace details                                                                              \
    {                                                                                              \
        template <typename T>                                                                      \
        using __##NAME##_impl = decltype(NEED_COMPILE);                                            \
    }                                                                                              \
    template <typename T>                                                                          \
    constexpr bool NAME = ::RAH2_NS::concepts::compiles<false, T, details::__##NAME##_impl>;
