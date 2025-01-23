#pragma once

#include "concepts.hpp"

#include <stdint.h>
#include <initializer_list>
#include <cassert>

#ifdef RAH2_USE_EASTL

#include <EASTL/type_traits.h>
#include <EASTL/iterator.h>
#include <EASTL/utility.h>

#else

#include <type_traits>
#include <iterator>
#include <utility>

#endif

#define RAH2_ITC_NS RAH2_NS
#define RAH2_ASSERT(expression) assert(expression)

#define RAH2_VALIDATE_COMPARE(CHECK)

#define RAH2_DEV_ASSERT assert

#define RAH2_INVOKE_0(FUNC) FUNC()
#define RAH2_INVOKE_1(FUNC, ARG) FUNC(ARG)
#define RAH2_INVOKE_2(FUNC, ARG1, ARG2) FUNC(ARG1, ARG2)

#if defined(_MSVC_LANG)
#define RAH2_CPP_VER _MSVC_LANG
#else
#define RAH2_CPP_VER __cplusplus
#endif

#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 202000L) || __cplusplus >= 202000L)
#define RAH2_CPP20 1
#else
#define RAH2_CPP20 0
#endif

#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 202300L) || __cplusplus >= 202300L)
#define RAH2_CPP23 1
#else
#define RAH2_CPP23 0
#endif

#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 201700L) || __cplusplus >= 201700L)
#define RAH2_CPP17 1
#else
#define RAH2_CPP17 0
#endif

// Functions that became constexpr in C++20
#if RAH2_CPP20
#define RAH2_CONSTEXPR20 constexpr
#else // ^^^ constexpr in C++20 and later / inline (not constexpr) in C++17 and earlier vvv
#define RAH2_CONSTEXPR20 inline
#endif // ^^^ inline (not constexpr) in C++17 and earlier ^^^

#if RAH2_CPP17
#define RAH2_NODISCARD [[nodiscard]]
#define RAH2_FALLTHROUGH [[fallthrough]]
#else
#define RAH2_NODISCARD
#if defined(__GNUC__) || defined(__clang__)
#define RAH2_FALLTHROUGH __attribute__((fallthrough))
#else
#define RAH2_FALLTHROUGH
#endif
#endif

#if !RAH2_CPP20
#include <ciso646>
#endif

#ifdef _MSC_VER
#define RAH2_EXT_WARNING_PUSH __pragma(warning(push, 0))
#define RAH2_EXT_WARNING_POP __pragma(warning(pop))
#elif defined(__GNUC__) || defined(__clang__)
#define RAH2_EXT_WARNING_PUSH _Pragma("GCC diagnostic push")
#define RAH2_EXT_WARNING_POP _Pragma("GCC diagnostic pop")
#else
#define RAH2_EXT_WARNING_PUSH
#define RAH2_EXT_WARNING_POP
#endif

// static_cast to rvalue reference
#define RAH2_MOV(...)                                                                              \
    static_cast<RAH2_STD::remove_reference_t<decltype(__VA_ARGS__)>&&>(__VA_ARGS__)

// static_cast to identity
#define RAH2_FWD(...) static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__)

namespace RAH2_NS
{
    // ***************************** <type_traits> traits *****************************************
#if !RAH2_INSIDE_EASTL
    template <class Base, class Derived>
    constexpr bool is_base_of_v = RAH2_STD::is_base_of<Base, Derived>::value;

    template <class T, class U>
    constexpr bool is_same_v = RAH2_STD::is_same<T, U>::value;

    template <class From, class To>
    constexpr bool is_convertible_v = RAH2_STD::is_convertible<From, To>::value;

    template <class T>
    constexpr bool is_pointer_v = RAH2_STD::is_pointer<T>::value;

    template <class T>
    constexpr bool is_lvalue_reference_v = RAH2_STD::is_lvalue_reference<T>::value;

    template <class T>
    constexpr bool is_integral_v = RAH2_STD::is_integral<T>::value;

    template <class T>
    constexpr bool is_signed_v = RAH2_STD::is_signed<T>::value;

    template <class T, class... Args>
    constexpr bool is_constructible_v = RAH2_STD::is_constructible<T, Args...>::value;
    template <class T, class... Args>
    constexpr bool is_trivially_constructible_v =
        RAH2_STD::is_trivially_constructible<T, Args...>::value;
    template <class T, class... Args>
    constexpr bool is_nothrow_constructible_v =
        RAH2_STD::is_nothrow_constructible<T, Args...>::value;

    template <class T>
    constexpr bool is_destructible_v = RAH2_STD::is_destructible<T>::value;
    template <class T>
    constexpr bool is_trivially_destructible_v = RAH2_STD::is_trivially_destructible<T>::value;
    template <class T>
    constexpr bool is_nothrow_destructible_v = RAH2_STD::is_nothrow_destructible<T>::value;

    template <class T>
    constexpr size_t alignment_of_v = RAH2_STD::alignment_of<T>::value;

    template <class T>
    constexpr bool is_reference_v = RAH2_STD::is_reference<T>::value;

    template <class T>
    constexpr bool is_rvalue_reference_v = RAH2_STD::is_rvalue_reference<T>::value;

    template <class T>
    struct is_scalar
        : RAH2_STD::integral_constant<
              bool,
              RAH2_STD::is_arithmetic<T>::value || RAH2_STD::is_enum<T>::value
                  || RAH2_STD::is_pointer<T>::value || RAH2_STD::is_member_pointer<T>::value
                  || RAH2_STD::is_null_pointer<T>::value>
    {
    };

    template <class T>
    constexpr bool is_scalar_v = is_scalar<T>::value;

    template <class T>
    struct remove_cvref
    {
        using type = RAH2_STD::remove_cv_t<RAH2_STD::remove_reference_t<T>>;
    };
    template <class T>
    using remove_cvref_t = typename remove_cvref<T>::type;
#endif

    struct view_base
    {
    };

    namespace details
    {
        template <typename T>
        struct is_initializer_list_impl
        {
            static constexpr bool value = false;
        };

        template <typename T>
        struct is_initializer_list_impl<std::initializer_list<T>>
        {
            static constexpr bool value = true;
        };
    } // namespace details
    template <typename T>
    constexpr bool is_initializer_list = details::is_initializer_list_impl<remove_cvref_t<T>>::value;

    // ****************************** <utility> helpers *******************************************
#if !RAH2_INSIDE_EASTL
    struct in_place_t
    {
        explicit in_place_t() = default;
    };
    constexpr in_place_t in_place{};

    template <class T>
    struct in_place_type_t
    {
        explicit in_place_type_t() = default;
    };

    template <class T>
    constexpr in_place_type_t<T> in_place_type{};

    template <size_t I>
    struct in_place_index_t
    {
        explicit in_place_index_t() = default;
    };

    template <size_t I>
    constexpr in_place_index_t<I> in_place_index{};

    // ****************************** <utility> functions *****************************************
    template <class T>
    constexpr RAH2_STD::add_const_t<T>& as_const(T& t) noexcept
    {
        return t;
    }
    template <class T>
    void as_const(T const&&) = delete;

#endif

    // **************************** <functional> Classes ******************************************
    namespace ranges
    {
        struct less
        {
            template <class T, class U>
            constexpr bool operator()(T&& t, U&& u) const
            {
                return t < u;
            }
        };

        struct greater
        {
            template <class T, class U>
            constexpr bool operator()(T&& t, U&& u) const
            {
                return t > u;
            }
        };

        struct equal_to
        {
            template <class T, class U>
            constexpr bool operator()(T&& t, U&& u) const
            {
                return static_cast<T&&>(t) == static_cast<U&&>(u);
            }
        };
    } // namespace ranges

    namespace details
    {
        struct identity
        {
            template <class T>
            constexpr T&& operator()(T&& t) const noexcept
            {
                // Faster than forward in debug mode
                return static_cast<T&&>(t);
            }
        };
    } // namespace details
#if !RAH2_INSIDE_EASTL
    using identity = details::identity;
#endif

    // ***************************** <concepts> concept *******************************************
    template <class Derived, class Base>
    constexpr bool derived_from =
        RAH2_NS::is_base_of_v<Base, Derived>
        && RAH2_NS::is_convertible_v<Derived const volatile*, Base const volatile*>;

    template <class T, class... Args>
    constexpr bool constructible_from =
        RAH2_NS::is_destructible_v<T> && RAH2_NS::is_constructible_v<T, Args...>;

    namespace details
    {
        template <class From, class To, bool Diagnostic = false>
        struct convertible_to_impl
        {
            template <class F>
            using can_static_cast = decltype(static_cast<To>(RAH2_STD::declval<F>()));

            static constexpr bool value = concepts::is_true_v<Diagnostic, is_convertible_v<From, To>>
                                          && concepts::compiles<Diagnostic, From, can_static_cast>;
        };

        template <class LHS, class RHS, bool Diagnostic = false>
        struct assignable_from_impl
        {
            template <class LHS_>
            using check_assign = RAH2_STD::enable_if_t<RAH2_NS::is_same_v<
                decltype(RAH2_STD::declval<LHS_>() = RAH2_STD::forward<RHS>(RAH2_STD::declval<RHS&&>())),
                LHS_>>;

            static constexpr bool value =
                concepts::is_true_v<Diagnostic, RAH2_NS::is_lvalue_reference_v<LHS>>
                && concepts::compiles<Diagnostic, LHS, check_assign>;
        };

        template <typename T, bool Diagnostic = false>
        struct default_initializable_impl
        {
            template <typename T2>
            using default_ctor = decltype(T2{});

            constexpr static bool value =
                concepts::is_true_v<Diagnostic, RAH2_NS::constructible_from<T>>
                && concepts::compiles<Diagnostic, T, default_ctor>;
        };
    } // namespace details

    template <class From, class To>
    constexpr bool convertible_to = details::convertible_to_impl<From, To>::value;

    template <class T>
    constexpr bool move_constructible =
        RAH2_NS::constructible_from<T, T> && RAH2_NS::convertible_to<T, T>;

    template <class U, typename V>
    constexpr bool same_as = RAH2_NS::is_same_v<U, V>;

    template <class LHS, class RHS>
    constexpr bool assignable_from = details::assignable_from_impl<LHS, RHS>::value;

    RAH2_MAKE_CONCEPT(swappable, RAH2_STD::swap(RAH2_STD::declval<T&>(), RAH2_STD::declval<T&>()));

#if !RAH2_INSIDE_EASTL
    template <class T>
    constexpr bool is_object_v = RAH2_STD::is_object<T>::value;
#endif

    template <class T>
    constexpr bool movable = RAH2_NS::is_object_v<T> && RAH2_NS::move_constructible<T>
                             && RAH2_NS::assignable_from<T&, T> && RAH2_NS::swappable<T>;

    template <class T>
    constexpr bool copy_constructible =
        RAH2_NS::move_constructible<T> && RAH2_NS::constructible_from<T, T&>
        && RAH2_NS::convertible_to<T&, T> && RAH2_NS::constructible_from<T, T const&>
        && RAH2_NS::convertible_to<T const&, T> && RAH2_NS::constructible_from<T, T const>
        && RAH2_NS::convertible_to<T const, T>;

    template <class T>
    constexpr bool copyable =
        RAH2_NS::copy_constructible<T> && RAH2_NS::movable<T> && RAH2_NS::assignable_from<T&, T&>
        && RAH2_NS::assignable_from<T&, T const&> && RAH2_NS::assignable_from<T&, T const>;

    template <typename T>
    constexpr bool default_initializable = details::default_initializable_impl<T>::value;

    template <class T, bool Diagnostic = false>
    constexpr bool semiregular = concepts::is_true_v<Diagnostic, RAH2_NS::copyable<T>>
                                 && details::default_initializable_impl<T, Diagnostic>::value;

    namespace details
    {
        template <class T, class U, bool Diagnostic = false>
        struct weakly_equality_comparable_with_impl
        {
            template <class T2>
            using check =
                decltype(!(RAH2_STD::declval<T2>() == RAH2_STD::declval<U>()), !(RAH2_STD::declval<T2>() != RAH2_STD::declval<U>()), !(RAH2_STD::declval<U>() == RAH2_STD::declval<T2>()), !(RAH2_STD::declval<U>() != RAH2_STD::declval<T2>()));

            constexpr static bool value = concepts::compiles<Diagnostic, T, check>;
        };
        template <class T, class U, bool Diagnostic = false>
        constexpr static bool weakly_equality_comparable_with =
            weakly_equality_comparable_with_impl<T, U, Diagnostic>::value;
    } // namespace details

    template <class T, bool Diagnostic = false>
    constexpr bool equality_comparable = details::weakly_equality_comparable_with<T, T, Diagnostic>;

    template <class T, bool Diagnostic = false>
    constexpr bool regular = //
        semiregular<T, Diagnostic> //
        && equality_comparable<T, Diagnostic>;

    template <class T>
    constexpr bool destructible = RAH2_NS::is_nothrow_destructible_v<T>;

    namespace details
    {
        template <typename T, typename U, bool Diagnostic = false>
        struct partially_ordered_with_impl
        {
            using NoRefT = RAH2_STD::remove_reference_t<T>;
            using NoRefU = RAH2_STD::remove_reference_t<U>;
            template <typename X, typename Y>
            using t_lesser_u =
                decltype(static_cast<bool>(RAH2_STD::declval<X>() < RAH2_STD::declval<Y>()));
            template <typename X, typename Y>
            using t_greater_u =
                decltype(static_cast<bool>(RAH2_STD::declval<X>() > RAH2_STD::declval<Y>()));
            template <typename X, typename Y>
            using t_lesserequal_u =
                decltype(static_cast<bool>(RAH2_STD::declval<X>() <= RAH2_STD::declval<Y>()));
            template <typename X, typename Y>
            using t_greaterequal_u =
                decltype(static_cast<bool>(RAH2_STD::declval<X>() >= RAH2_STD::declval<Y>()));

            static constexpr bool value =
                concepts::compiles2<Diagnostic, NoRefT, NoRefU, t_lesser_u>
                && concepts::compiles2<Diagnostic, NoRefT, NoRefU, t_greater_u>
                && concepts::compiles2<Diagnostic, NoRefT, NoRefU, t_lesserequal_u>
                && concepts::compiles2<Diagnostic, NoRefT, NoRefU, t_greaterequal_u>
                && concepts::compiles2<Diagnostic, NoRefU, NoRefT, t_lesser_u>
                && concepts::compiles2<Diagnostic, NoRefU, NoRefT, t_greater_u>
                && concepts::compiles2<Diagnostic, NoRefU, NoRefT, t_lesserequal_u>
                && concepts::compiles2<Diagnostic, NoRefU, NoRefT, t_greaterequal_u>;
        };
        template <typename T, typename U>
        constexpr bool partially_ordered_with = partially_ordered_with_impl<T, U>::value;

        template <class T, bool Diagnostic = false>
        struct totally_ordered_impl
        {
            static constexpr bool value =
                concepts::is_true_v<Diagnostic, RAH2_NS::equality_comparable<T>>
                && details::partially_ordered_with_impl<T, T, Diagnostic>::value;
        };

    } // namespace details

    template <class T>
    constexpr bool totally_ordered = details::totally_ordered_impl<T>::value;

    // **************************** <iterator> traits *********************************************
#if defined(EASTL_STD_ITERATOR_CATEGORY_ENABLED) and EASTL_STD_ITERATOR_CATEGORY_ENABLED
    using std::bidirectional_iterator_tag;
    using std::forward_iterator_tag;
    using std::input_iterator_tag;
    using std::output_iterator_tag;
    using std::random_access_iterator_tag;
#if RAH2_CPP20
    using std::contiguous_iterator_tag;
#else
    struct contiguous_iterator_tag : std::random_access_iterator_tag
    {
    };
#endif

#else
    using RAH2_STD::bidirectional_iterator_tag;
    using RAH2_STD::forward_iterator_tag;
    using RAH2_STD::input_iterator_tag;
    using RAH2_STD::output_iterator_tag;
    using RAH2_STD::random_access_iterator_tag;
#if RAH2_CPP20 or defined(EASTL_VERSION)
    using RAH2_STD::contiguous_iterator_tag;
#else
    struct contiguous_iterator_tag : RAH2_STD::random_access_iterator_tag
    {
    };
#endif
#endif

    // TODO : Implement iter_difference_t
    template <typename I>
    using iter_difference_t = ptrdiff_t;

    template <class T>
    using iter_value_t = typename RAH2_STD::iterator_traits<T>::value_type;

    template <class T>
    using iter_reference_t = decltype(*RAH2_STD::declval<T&>());

    template <typename T>
    using iter_rvalue_reference_t = decltype(RAH2_STD::move(*RAH2_STD::declval<T&>()));

    template <class T>
    using iter_const_reference_t = decltype(*RAH2_STD::declval<T const&>());

    // **************************** <iterator> Customization Point Object *************************
    namespace ranges
    {
        struct iter_move_cpo
        {
            template <class T>
            constexpr auto operator()(T&& t) const -> RAH2_STD::remove_reference_t<decltype(*t)>
            {
                return RAH2_STD::move(*t);
            }
        };
        constexpr iter_move_cpo iter_move;

        struct iter_swap_cpo
        {
            template <class ForwardIterator1, class ForwardIterator2>
            constexpr void operator()(ForwardIterator1 left, ForwardIterator2 right) const
            {
                RAH2_STD::swap(*left, *right);
            }
        };
        constexpr iter_swap_cpo iter_swap;
    } // namespace ranges

    // **************************** <iterator> concepts *******************************************
    template <class S, class I, bool Diagnostic = false>
    static constexpr bool disable_sized_sentinel_for = false;

    namespace details
    {
        template <typename I>
        using has_iterator_iterator_concept = typename I::iterator_concept;

        template <typename I>
        using has_iterator_traits_iterator_concept =
            typename RAH2_STD::iterator_traits<I>::iterator_concept;

        template <typename I>
        using has_iterator_traits_iterator_category =
            typename RAH2_STD::iterator_traits<I>::iterator_category;

        template <typename I, typename = void>
        struct iterator_concept_impl;

        template <typename I>
        struct iterator_concept_impl<I, RAH2_STD::enable_if_t<RAH2_NS::is_pointer_v<I>>>
        {
            using type = RAH2_NS::contiguous_iterator_tag;
        };

        template <typename I>
        struct iterator_concept_impl<
            I,
            RAH2_STD::enable_if_t<
                !RAH2_NS::is_pointer_v<I> && concepts::compiles<false, I, has_iterator_iterator_concept>>>
        {
            using type = typename I::iterator_concept;
        };

        template <typename I>
        struct iterator_concept_impl<
            I,
            RAH2_STD::enable_if_t<
                !RAH2_NS::is_pointer_v<I> && !concepts::compiles<false, I, has_iterator_iterator_concept>
                && concepts::compiles<false, I, has_iterator_traits_iterator_concept>>>
        {
            using type = typename RAH2_STD::iterator_traits<I>::iterator_concept;
        };

        template <typename I>
        struct iterator_concept_impl<
            I,
            RAH2_STD::enable_if_t<
                !RAH2_NS::is_pointer_v<I> && !concepts::compiles<false, I, has_iterator_iterator_concept>
                && !concepts::compiles<false, I, has_iterator_traits_iterator_concept>
                && concepts::compiles<false, I, has_iterator_traits_iterator_category>>>
        {
            using type = typename RAH2_STD::iterator_traits<I>::iterator_category;
        };

        template <typename I>
        using iterator_concept = typename iterator_concept_impl<I>::type;

        template <class Out, class T, bool Diagnostic = false>
        struct indirectly_writable_impl
        {
            template <typename O>
            using can_indirect_assign1 =
                decltype(*RAH2_STD::declval<O&&>() = RAH2_STD::declval<T>());
            template <typename O>
            using can_indirect_assign2 = decltype(*RAH2_STD::declval<O>() = RAH2_STD::declval<T>());

            static constexpr bool value =
                concepts::compiles<Diagnostic, Out, can_indirect_assign1>
                && concepts::compiles<Diagnostic, Out, can_indirect_assign2>;
        };

        template <typename In, bool Diagnostic = false>
        struct indirectly_readable_impl
        {
            template <class T>
            using has_value = iter_value_t<T>;
            template <class T>
            using has_ref = iter_reference_t<T>;
            template <class T>
            using has_rval_ref = iter_rvalue_reference_t<T>;
            template <class T>
            using deref = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(*RAH2_STD::declval<T>()), iter_reference_t<T>>>;
            template <class T>
            using itermove = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(iter_move(RAH2_STD::declval<T>())), iter_rvalue_reference_t<T>>>;

            // TODO re-add checks
            static constexpr bool value =
                // concepts::compiles<In, has_value> &&
                // concepts::compiles<In, has_ref> &&
                // concepts::compiles<In, has_rval_ref> &&
                concepts::compiles<Diagnostic, In, deref>
                // && concepts::compiles<In, itermove>
                ;
        };

        template <class In, class Out, bool Diagnostic = false>
        struct indirectly_movable_impl
        {
            template <typename In2>
            using indirectly_writable = RAH2_STD::enable_if_t<
                indirectly_writable_impl<Out, RAH2_NS::iter_rvalue_reference_t<In2>>::value>;

            static constexpr bool value =
                concepts::is_true_v<Diagnostic, indirectly_readable_impl<In>::value>
                && concepts::compiles<Diagnostic, In, indirectly_writable>;
        };

        template <class In, class Out, bool Diagnostic = false>
        struct indirectly_movable_storable_impl
        {
            template <typename In2>
            using indirectly_writable =
                RAH2_STD::enable_if_t<indirectly_writable_impl<Out, RAH2_NS::iter_value_t<In2>>::value>;
            template <typename In2>
            using movable = RAH2_STD::enable_if_t<RAH2_NS::movable<RAH2_NS::iter_value_t<In2>>>;
            template <typename In2>
            using constructible_from = RAH2_STD::enable_if_t<
                RAH2_NS::constructible_from<RAH2_NS::iter_value_t<In2>, RAH2_NS::iter_rvalue_reference_t<In2>>>;
            template <typename In2>
            using assignable_from = RAH2_STD::enable_if_t<
                RAH2_NS::assignable_from<RAH2_NS::iter_value_t<In2>&, RAH2_NS::iter_rvalue_reference_t<In2>>>;

            static constexpr bool value = indirectly_movable_impl<In, Out, Diagnostic>::value
                                          && concepts::compiles<Diagnostic, In, indirectly_writable>
                                          && concepts::compiles<Diagnostic, In, movable>
                                          && concepts::compiles<Diagnostic, In, constructible_from>
                                          && concepts::compiles<Diagnostic, In, assignable_from>;
        };

        template <class I1, class I2 = I1, bool Diagnostic = false>
        struct indirectly_swappable_impl
        {
            template <class U1>
            using can_swap_1_1 = decltype(RAH2_NS::ranges::iter_swap(
                RAH2_STD::declval<U1 const>(), RAH2_STD::declval<U1 const>()));
            template <class U1>
            using can_swap_1_2 = decltype(RAH2_NS::ranges::iter_swap(
                RAH2_STD::declval<U1 const>(), RAH2_STD::declval<I2 const>()));
            template <class U1>
            using can_swap_2_1 = decltype(RAH2_NS::ranges::iter_swap(
                RAH2_STD::declval<I1 const>(), RAH2_STD::declval<U1 const>()));
            template <class U2>
            using can_swap_2_2 = decltype(RAH2_NS::ranges::iter_swap(
                RAH2_STD::declval<U2 const>(), RAH2_STD::declval<U2 const>()));

            static constexpr bool value =
                concepts::is_true_v<Diagnostic, indirectly_readable_impl<I1>::value>
                && concepts::is_true_v<Diagnostic, indirectly_readable_impl<I2>::value>
                && concepts::compiles<Diagnostic, I1, can_swap_1_1>
                && concepts::compiles<Diagnostic, I1, can_swap_1_2>
                && concepts::compiles<Diagnostic, I1, can_swap_2_1>
                && concepts::compiles<Diagnostic, I2, can_swap_2_2>;
        };

        template <typename I, bool Diagnostic = false>
        struct weakly_incrementable_impl
        {
            template <class T>
            using diff_is_signed_integer = RAH2_STD::enable_if_t<
                RAH2_NS::is_integral_v<iter_difference_t<T>>
                && RAH2_NS::is_signed_v<iter_difference_t<T>>>;
            template <class T>
            using incr_type =
                RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(++RAH2_STD::declval<T>()), T&>>;

            template <class T>
            using can_post_incr = decltype(RAH2_STD::declval<T&>()++);

            static constexpr bool value =
                concepts::compiles<Diagnostic, I, diff_is_signed_integer>
                // && concepts::is_true_v<Diagnostic, movable<I>>
                && concepts::is_true_v<Diagnostic, RAH2_STD::is_move_constructible<I>::value> // eastl::back_inserter is not move_assignable
                && concepts::compiles<Diagnostic, I, can_post_incr>;
        };

        template <typename I, bool Diagnostic = false>
        struct incrementable_impl
        {
            template <typename U>
            using check_incr =
                RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U&>()++), U>>;

            static constexpr bool value = regular<I, Diagnostic>
                                          && weakly_incrementable_impl<I, Diagnostic>::value
                                          && concepts::compiles<Diagnostic, I, check_incr>;
        };

        template <typename R, bool Diagnostic = false>
        struct input_or_output_iterator_impl
        {
            template <class T>
            using can_ref =
                RAH2_STD::enable_if_t<not RAH2_NS::is_same_v<decltype(*RAH2_STD::declval<T>()), void>>;

            static constexpr bool value = weakly_incrementable_impl<R, Diagnostic>::value
                                          && concepts::compiles<Diagnostic, R, can_ref>;
        };

        template <class I, class T, bool Diagnostic = false>
        struct output_iterator_impl
        {
            template <typename U>
            using can_assign_incr = decltype(*RAH2_STD::declval<U&>()++ = RAH2_STD::declval<T&&>());

            static constexpr bool value =
                concepts::is_true_v<Diagnostic, input_or_output_iterator_impl<I>::value>
                && indirectly_writable_impl<I, T, Diagnostic>::value
                && RAH2_NS::concepts::compiles<Diagnostic, I, can_assign_incr>;
        };

        template <typename I, bool Diagnostic = false>
        struct input_iterator_impl
        {
            template <typename I2>
            using derived_from_input = RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<iterator_concept<I2>, RAH2_NS::input_iterator_tag>
                || RAH2_NS::derived_from<iterator_concept<I2>, RAH2_NS::forward_iterator_tag>>;

            static constexpr bool value =
                concepts::is_true_v<Diagnostic, input_or_output_iterator_impl<I>::value>
                && indirectly_readable_impl<I, Diagnostic>::value
                && concepts::compiles<Diagnostic, I, iterator_concept>
                && concepts::compiles<Diagnostic, I, derived_from_input>;
        };

        template <class S, class I, bool Diagnostic = false>
        struct sentinel_for_impl
        {
            static constexpr bool value =
                concepts::is_true_v<Diagnostic, RAH2_NS::semiregular<S, Diagnostic>>
                && concepts::is_true_v<Diagnostic, input_or_output_iterator_impl<I>::value>
                && weakly_equality_comparable_with_impl<I, S, Diagnostic>::value;
        };

        template <typename I, bool Diagnostic = false>
        struct forward_iterator_impl
        {
            template <typename U>
            using has_forward_tag = RAH2_STD::enable_if_t<
                derived_from<details::iterator_concept<U>, RAH2_STD::forward_iterator_tag>>;

            static constexpr bool value = input_iterator_impl<I, Diagnostic>::value
                                          && concepts::compiles<Diagnostic, I, has_forward_tag>
                                          && incrementable_impl<I, Diagnostic>::value
                                          && sentinel_for_impl<I, I, Diagnostic>::value;
        };

        template <class S, class I, bool Diagnostic = false>
        struct sized_sentinel_for_impl
        {
            template <class S2>
            using less_s_i =
                decltype((RAH2_STD::declval<S2>() - RAH2_STD::declval<I>()) + (RAH2_STD::declval<I>() - RAH2_STD::declval<S2>()));
            static constexpr bool value =
                sentinel_for_impl<S, I, Diagnostic>::value
                && concepts::is_true_v<
                    Diagnostic,
                    !RAH2_NS::disable_sized_sentinel_for<RAH2_STD::remove_cv_t<S>, RAH2_STD::remove_cv_t<I>>>
                && concepts::compiles<Diagnostic, S, less_s_i>;
        };

        template <typename I, bool Diagnostic = false>
        struct bidirectional_iterator_impl
        {
            template <typename U>
            using can_pre_decr =
                RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(--RAH2_STD::declval<U&>()), U&>>;
            template <typename U>
            using can_post_decr =
                RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U&>()--), U>>;
            template <typename U>
            using has_bidir_cat = RAH2_STD::enable_if_t<
                derived_from<details::iterator_concept<U>, RAH2_STD::bidirectional_iterator_tag>>;

            static constexpr bool value = forward_iterator_impl<I, Diagnostic>::value
                                          && concepts::compiles<Diagnostic, I, has_bidir_cat>
                                          && concepts::compiles<Diagnostic, I, can_pre_decr>
                                          && concepts::compiles<Diagnostic, I, can_post_decr>;
        };

        template <typename I, bool Diagnostic = false>
        struct random_access_iterator_impl
        {
            template <typename U>
            using addEqual = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U&>() += RAH2_NS::iter_difference_t<U>()), U&>>;

            template <typename U>
            using add = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U>() + RAH2_NS::iter_difference_t<U>()), U>>;

            template <typename U>
            using add2 = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(RAH2_NS::iter_difference_t<U>() + RAH2_STD::declval<U>()), U>>;

            template <typename U>
            using subEqual = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U&>() -= RAH2_NS::iter_difference_t<U>()), U&>>;

            template <typename U>
            using sub = RAH2_STD::enable_if_t<
                RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U>() - RAH2_NS::iter_difference_t<U>()), U>>;

            template <typename U>
            using arr = RAH2_STD::enable_if_t<RAH2_NS::is_same_v<
                decltype(RAH2_STD::declval<U>()[RAH2_NS::iter_difference_t<U>()]),
                RAH2_NS::iter_reference_t<U>>>;

            template <typename U>
            using has_random_access_cat = RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<details::iterator_concept<U>, RAH2_STD::random_access_iterator_tag>>;

            static constexpr bool value =
                bidirectional_iterator_impl<I, Diagnostic>::value
                && concepts::compiles<Diagnostic, I, has_random_access_cat>
                && totally_ordered_impl<I, Diagnostic>::value
                && sized_sentinel_for_impl<I, I, Diagnostic>::value
                && concepts::compiles<Diagnostic, I, addEqual>
                && concepts::compiles<Diagnostic, I, add> && concepts::compiles<Diagnostic, I, add2>
                && concepts::compiles<Diagnostic, I, subEqual>
                && concepts::compiles<Diagnostic, I, sub> && concepts::compiles<Diagnostic, I, arr>;
        };

        template <typename I, bool Diagnostic = false>
        struct contiguous_iterator_impl
        {
            template <typename U>
            using has_contig_tag = RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<iterator_concept<U>, RAH2_NS::contiguous_iterator_tag>>;
            template <typename U>
            using same_value_and_ref = RAH2_STD::enable_if_t<RAH2_NS::same_as<
                RAH2_NS::iter_value_t<U>,
                RAH2_NS::remove_cvref_t<RAH2_NS::iter_reference_t<U>>>>;
            template <typename U>
            using ref_is_lvalue =
                RAH2_STD::enable_if_t<RAH2_NS::is_lvalue_reference_v<RAH2_NS::iter_reference_t<U>>>;

            static constexpr bool value =
                RAH2_NS::details::random_access_iterator_impl<I, Diagnostic>::value
                && concepts::compiles<Diagnostic, I, has_contig_tag>
                && concepts::compiles<Diagnostic, I, ref_is_lvalue>
                && concepts::compiles<Diagnostic, I, same_value_and_ref>;
        };

        template <typename In, typename Out, bool Diagnostic = false>
        struct indirectly_copyable_impl
        {
            template <typename In2, typename Out2>
            using can_indirect_assign1 =
                decltype(*RAH2_STD::declval<Out2&&>() = RAH2_STD::declval<RAH2_NS::iter_reference_t<In2>>());
            template <typename In2, typename Out2>
            using can_indirect_assign2 =
                decltype(*RAH2_STD::declval<Out2>() = RAH2_STD::declval<RAH2_NS::iter_reference_t<In2>>());

            static constexpr bool is_indirectly_writable =
                concepts::compiles2<Diagnostic, In, Out, can_indirect_assign1>
                && concepts::compiles2<Diagnostic, In, Out, can_indirect_assign2>;

            static constexpr bool value = details::indirectly_readable_impl<remove_cvref_t<In>>::value
                                          && concepts::is_true_v<Diagnostic, is_indirectly_writable>;
        };
    } // namespace details

    template <class Out, class T>
    constexpr bool indirectly_writable = details::indirectly_writable_impl<Out, T>::value;

    template <typename T>
    constexpr bool indirectly_readable = details::indirectly_readable_impl<remove_cvref_t<T>>::value;

    template <class In, class Out>
    constexpr bool indirectly_movable = details::indirectly_movable_impl<In, Out>::value;

    template <class In, class Out>
    constexpr bool indirectly_movable_storable =
        details::indirectly_movable_storable_impl<In, Out>::value;

    template <class I1, class I2 = I1>
    constexpr bool indirectly_swappable = details::indirectly_swappable_impl<I1, I2>::value;

    template <typename T>
    constexpr bool weakly_incrementable = details::weakly_incrementable_impl<T>::value;

    template <typename I>
    constexpr bool incrementable = details::incrementable_impl<I>::value;

    template <typename T>
    constexpr bool input_or_output_iterator = details::input_or_output_iterator_impl<T>::value;

    template <class I, class T>
    constexpr bool output_iterator = details::output_iterator_impl<I, T>::value;

    template <typename T>
    constexpr bool input_iterator = details::input_iterator_impl<T>::value;

    template <class S, class I>
    constexpr bool sentinel_for = details::sentinel_for_impl<S, I>::value;

    template <typename I>
    constexpr bool forward_iterator = details::forward_iterator_impl<I>::value;

    template <class I>
    constexpr bool permutable =
        RAH2_NS::forward_iterator<I> && RAH2_NS::indirectly_movable_storable<I, I>
        && RAH2_NS::indirectly_swappable<I, I>;

    template <class S, class I>
    static constexpr bool sized_sentinel_for = details::sized_sentinel_for_impl<S, I>::value;

    template <typename I>
    constexpr bool bidirectional_iterator =
        details::bidirectional_iterator_impl<remove_cvref_t<I>>::value;

    template <typename I>
    constexpr bool random_access_iterator =
        details::random_access_iterator_impl<remove_cvref_t<I>>::value;

    template <typename I>
    static constexpr bool contiguous_iterator = details::contiguous_iterator_impl<I>::value;

    template <typename In, typename Out>
    constexpr bool indirectly_copyable = details::indirectly_copyable_impl<In, Out>::value;

    // **************************** <ranges> access ***********************************************
    namespace ranges
    {
        namespace details
        {
            RAH2_MAKE_CONCEPT(has_begin_member, (RAH2_STD::declval<T>().begin()));
            RAH2_MAKE_CONCEPT(has_begin_ADL, (begin(RAH2_STD::declval<T>())));
            RAH2_MAKE_CONCEPT(has_end_member, (RAH2_STD::declval<T>().end()));
            RAH2_MAKE_CONCEPT(has_end_ADL, (end(RAH2_STD::declval<T>())));
            RAH2_MAKE_CONCEPT(
                has_rbegin_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().rbegin())
            RAH2_MAKE_CONCEPT(
                has_rbegin_ADL, rbegin(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            RAH2_MAKE_CONCEPT(
                has_rend_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().rend())
            RAH2_MAKE_CONCEPT(has_rend_ADL, rend(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            RAH2_MAKE_CONCEPT(
                has_size_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().size())
            RAH2_MAKE_CONCEPT(has_size_ADL, size(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            RAH2_MAKE_CONCEPT(
                has_data_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().data())
            RAH2_MAKE_CONCEPT(has_data_ADL, data(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            RAH2_MAKE_CONCEPT(
                has_empty_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().empty())

            struct begin_impl
            {
                template <class T, size_t N>
                RAH2_CONSTEXPR20 T* operator()(T (&array)[N]) const
                {
                    return array;
                }

                template <class T, size_t N>
                RAH2_CONSTEXPR20 T const* operator()(T const (&array)[N]) const
                {
                    return array;
                }

                template <typename R, RAH2_STD::enable_if_t<has_begin_member<R>>* = nullptr>
                RAH2_CONSTEXPR20 auto operator()(R&& range) const
                {
                    return range.begin();
                }

                template <typename R, RAH2_STD::enable_if_t<not has_begin_member<R> && has_begin_ADL<R>>* = nullptr>
                RAH2_CONSTEXPR20 auto operator()(R&& range) const
                {
                    return begin(range);
                }
            };

            struct end_impl
            {
                template <class T, size_t N>
                RAH2_CONSTEXPR20 T* operator()(T (&array)[N]) const noexcept
                {
                    return array + N;
                }

                template <class T, size_t N>
                RAH2_CONSTEXPR20 T const* operator()(T const (&array)[N]) const noexcept
                {
                    return array + N;
                }

                template <typename R, RAH2_STD::enable_if_t<has_end_member<R>>* = nullptr>
                RAH2_CONSTEXPR20 auto operator()(R&& range) const
                {
                    return range.end();
                }

                template <typename R, RAH2_STD::enable_if_t<not has_end_member<R> and has_end_ADL<R>>* = nullptr>
                RAH2_CONSTEXPR20 auto operator()(R&& range) const
                {
                    return end(range);
                }
            };

            struct cbegin_impl
            {
                template <class T, size_t N>
                RAH2_CONSTEXPR20 T const* operator()(T const (&array)[N]) const
                {
                    return array;
                }

                template <typename R>
                RAH2_CONSTEXPR20 auto operator()(R const& range) const
                {
                    return range.begin();
                }
            };

            struct cend_impl
            {
                template <class T, size_t N>
                RAH2_CONSTEXPR20 T const* operator()(T const (&array)[N]) const noexcept
                {
                    return array + N;
                }

                template <typename R>
                RAH2_CONSTEXPR20 auto operator()(R const& range) const
                {
                    return range.end();
                }
            };

            template <typename Ptr, typename Iter>
            struct unwraped_pointers
            {
                Ptr iterator;
                Ptr sentinel;
                Iter first_iter;
                Iter wrap_iterator(Ptr last)
                {
                    return first_iter + (last - iterator);
                }
            };

            template <typename I, typename S>
            struct unwraped_iterators
            {
                I iterator;
                S sentinel;
                I wrap_iterator(I&& last)
                {
                    return RAH2_STD::move(last);
                }
            };

            template <
                typename I,
                typename S,
                RAH2_STD::enable_if_t<
                    contiguous_iterator<RAH2_STD::remove_reference_t<I>>
                    && sized_sentinel_for<RAH2_STD::remove_reference_t<S>, RAH2_STD::remove_reference_t<I>>>* =
                    nullptr>
            unwraped_pointers<
                RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>*,
                RAH2_STD::remove_reference_t<I>>
            unwrap(I&& it, S&& s)
            {
                if (it != s)
                {
                    auto begin_it = &(*it);
                    auto const range_size = s - it;
                    return {begin_it, begin_it + range_size, RAH2_STD::forward<I>(it)};
                }
                else
                {
                    return {nullptr, nullptr, RAH2_STD::forward<I>(it)};
                }
            }
            template <
                typename I,
                typename S,
                RAH2_STD::enable_if_t<
                    RAH2_STD::is_assignable<RAH2_NS::remove_cvref_t<I>, S>::value
                    && not(
                        contiguous_iterator<RAH2_STD::remove_reference_t<I>>
                        && sized_sentinel_for<RAH2_STD::remove_reference_t<S>, RAH2_STD::remove_reference_t<I>>)>* =
                    nullptr>
            unwraped_iterators<RAH2_STD::remove_reference_t<I>, RAH2_STD::remove_reference_t<S>>
            unwrap(I&& it, S&& s)
            {
                I it_end = RAH2_STD::move(s);
                return {RAH2_STD::forward<I>(it), RAH2_STD::move(it_end)};
            }
            template <
                typename I,
                typename S,
                RAH2_STD::enable_if_t<
                    not RAH2_STD::is_assignable<I, S>::value
                    && not(
                        contiguous_iterator<RAH2_STD::remove_reference_t<I>>
                        && sized_sentinel_for<RAH2_STD::remove_reference_t<S>, RAH2_STD::remove_reference_t<I>>)>* =
                    nullptr>
            unwraped_iterators<RAH2_STD::remove_reference_t<I>, RAH2_STD::remove_reference_t<S>>
            unwrap(I&& it, S&& s)
            {
                return {RAH2_STD::forward<I>(it), RAH2_STD::forward<S>(s)};
            }

            template <typename MemPtr>
            struct call_member_pointer
            {
                MemPtr mem_ptr{};
                template <typename Class>
                auto operator()(Class&& self) const -> decltype(self.*mem_ptr)
                {
                    return self.*mem_ptr;
                }
            };

            template <
                typename T,
                RAH2_STD::enable_if_t<
                    RAH2_STD::is_member_pointer<RAH2_STD::remove_reference_t<T>>::value>* = nullptr>
            call_member_pointer<RAH2_STD::remove_reference_t<T>> wrap_unary(T&& mem_ptr)
            {
                return call_member_pointer<RAH2_STD::remove_reference_t<T>>{mem_ptr};
            }

            template <
                typename T,
                RAH2_STD::enable_if_t<
                    !RAH2_STD::is_member_pointer<RAH2_STD::remove_reference_t<T>>::value>* = nullptr>
            T wrap_unary(T&& func)
            {
                return RAH2_STD::forward<T>(func);
            }

            template <typename T>
            auto move_unary(T&& func) -> decltype(wrap_unary(RAH2_STD::move(func)))
            {
                return wrap_unary(RAH2_STD::move(func));
            }

            template <
                typename Pred,
                typename Proj,
                RAH2_STD::enable_if_t<
                    RAH2_NS::is_same_v<RAH2_NS::details::identity, RAH2_NS::remove_cvref_t<Proj>>>* = nullptr>
            auto wrap_pred_proj(Pred&& pred, Proj&&)
            {
                return RAH2_STD::forward<Pred>(pred);
            }

            template <
                typename Pred,
                typename Proj1,
                typename Proj2,
                RAH2_STD::enable_if_t<
                    RAH2_NS::is_same_v<RAH2_NS::details::identity, RAH2_NS::remove_cvref_t<Proj1>>
                    and RAH2_NS::is_same_v<RAH2_NS::details::identity, RAH2_NS::remove_cvref_t<Proj2>>>* = nullptr>
            auto wrap_pred_proj(Pred&& pred, Proj1&&, Proj2&&)
            {
                return RAH2_STD::forward<Pred>(pred);
            }

            template <typename Pred, typename Proj>
            struct wrap_pred_memptr_fn
            {
                Pred pred;
                Proj proj;
                template <typename V>
                auto operator()(V&& v) const
                {
                    return pred(RAH2_FWD(v).*proj);
                }
                template <typename V1, typename V2>
                auto operator()(V1&& v1, V2&& v2) const
                {
                    return pred(RAH2_FWD(v1).*proj, RAH2_FWD(v2).*proj);
                }

                operator Pred() &&
                {
                    return RAH2_STD::move(pred);
                }
            };

            template <
                typename Pred,
                typename Proj,
                RAH2_STD::enable_if_t<
                    RAH2_STD::is_member_pointer<RAH2_STD::remove_reference_t<Proj>>::value>* = nullptr>
            auto wrap_pred_proj(Pred&& pred, Proj&& proj)
            {
                return wrap_pred_memptr_fn<
                    decltype(wrap_unary(RAH2_FWD(pred))),
                    RAH2_STD::remove_reference_t<Proj>>{wrap_unary(RAH2_FWD(pred)), RAH2_FWD(proj)};
            }

            template <typename Pred, typename Proj>
            struct wrap_pred_proj_fn
            {
                Pred pred;
                Proj proj;
                template <typename V>
                auto operator()(V&& v) const
                {
                    return pred(proj(RAH2_FWD(v)));
                }
                template <typename V1, typename V2>
                auto operator()(V1&& v1, V2&& v2) const
                {
                    return pred(proj(RAH2_FWD(v1)), proj(RAH2_FWD(v2)));
                }

                operator Pred() &&
                {
                    return RAH2_STD::move(pred);
                }
            };

            template <
                typename Pred,
                typename Proj,
                RAH2_STD::enable_if_t<
                    !RAH2_NS::is_same_v<RAH2_NS::details::identity, RAH2_NS::remove_cvref_t<Proj>>
                    and !RAH2_STD::is_member_pointer<RAH2_STD::remove_reference_t<Proj>>::value>* = nullptr>
            auto wrap_pred_proj(Pred&& pred, Proj&& proj)
            {
                return wrap_pred_proj_fn<
                    decltype(wrap_unary(RAH2_FWD(pred))),
                    decltype(wrap_unary(RAH2_FWD(proj)))>{
                    wrap_unary(RAH2_FWD(pred)), wrap_unary(RAH2_FWD(proj))};
            }

            template <typename Pred, typename Proj>
            struct wrap_pred_proj_value_fn
            {
                Pred pred;
                Proj proj;
                template <typename V>
                auto operator()(V&& v) const
                {
                    return pred(proj(RAH2_FWD(v)));
                }
                template <typename V1, typename V2>
                auto operator()(V1&& v1, V2&& v2) const
                {
                    return pred(proj(RAH2_FWD(v1)), RAH2_FWD(v2));
                }

                operator Pred() &&
                {
                    return RAH2_STD::move(pred);
                }
            };

            template <typename Pred, typename Proj>
            auto wrap_pred_proj_value(Pred&& pred, Proj&& proj)
            {
                return wrap_pred_proj_value_fn<
                    decltype(wrap_unary(RAH2_FWD(pred))),
                    decltype(wrap_unary(RAH2_FWD(proj)))>{
                    wrap_unary(RAH2_FWD(pred)), wrap_unary(RAH2_FWD(proj))};
            }

            template <typename Pred, typename Proj>
            struct wrap_pred_value_proj_fn
            {
                Pred pred;
                Proj proj;
                template <typename V>
                auto operator()(V&& v) const
                {
                    return pred(proj(RAH2_FWD(v)));
                }
                template <typename V1, typename V2>
                auto operator()(V1&& v1, V2&& v2) const
                {
                    return pred(RAH2_FWD(v1), proj(RAH2_FWD(v2)));
                }

                operator Pred() &&
                {
                    return RAH2_STD::move(pred);
                }
            };

            template <typename Pred, typename Proj>
            auto wrap_pred_value_proj(Pred&& pred, Proj&& proj)
            {
                return wrap_pred_value_proj_fn<
                    decltype(wrap_unary(RAH2_FWD(pred))),
                    decltype(wrap_unary(RAH2_FWD(proj)))>{
                    wrap_unary(RAH2_FWD(pred)), wrap_unary(RAH2_FWD(proj))};
            }

            template <typename Pred, typename Proj1, typename Proj2>
            struct wrap_pred_proj_fn_proj_fn
            {
                Pred pred;
                Proj1 proj1;
                Proj2 proj2;
                template <typename V1, typename V2>
                auto operator()(V1&& v1, V2&& v2) const
                {
                    return pred(proj1(RAH2_FWD(v1)), proj2(RAH2_FWD(v2)));
                }

                operator Pred() &&
                {
                    return RAH2_STD::move(pred);
                }
            };

            template <
                typename Pred,
                typename Proj1,
                typename Proj2,
                RAH2_STD::enable_if_t<not(
                    RAH2_NS::is_same_v<RAH2_NS::details::identity, RAH2_NS::remove_cvref_t<Proj1>>
                    and RAH2_NS::is_same_v<RAH2_NS::details::identity, RAH2_NS::remove_cvref_t<Proj2>>)>* = nullptr>
            auto wrap_pred_proj(Pred&& pred, Proj1&& proj1, Proj2&& proj2)
            {
                return wrap_pred_proj_fn_proj_fn<
                    decltype(wrap_unary(RAH2_FWD(pred))),
                    decltype(wrap_unary(RAH2_FWD(proj1))),
                    decltype(wrap_unary(RAH2_FWD(proj2)))>{
                    wrap_unary(RAH2_FWD(pred)),
                    wrap_unary(RAH2_FWD(proj1)),
                    wrap_unary(RAH2_FWD(proj2))};
            }

        } // namespace details

        constexpr auto begin = details::begin_impl();
        constexpr auto end = details::end_impl();
        constexpr auto cbegin = details::cbegin_impl();
        constexpr auto cend = details::cend_impl();

        // **************************** <ranges> traits ***********************************************
        template <typename T>
        using iterator_t = decltype(begin(RAH2_STD::declval<T>()));

        template <typename T>
        using sentinel_t = decltype(end(RAH2_STD::declval<T&>()));

        // **************************** <ranges> concepts *********************************************
        namespace details
        {
            template <typename R, bool Diagnostic = false>
            struct range_impl
            {
                template <typename R2>
                using has_begin = decltype(ranges::begin(RAH2_STD::declval<R2>()));
                template <typename R2>
                using has_end = decltype(ranges::end(RAH2_STD::declval<R2>()));

                static constexpr bool value = concepts::compiles<Diagnostic, R, has_begin>
                                              && concepts::compiles<Diagnostic, R, has_end>;
            };

            template <class T, bool Diagnostic = false>
            struct common_range_impl
            {
                template <typename U>
                using iter_is_sent =
                    RAH2_STD::enable_if_t<RAH2_NS::is_same_v<iterator_t<U>, sentinel_t<U>>>;

                static constexpr bool value = range_impl<T, Diagnostic>::value
                                              && concepts::compiles<Diagnostic, T, iter_is_sent>;
            };
        } // namespace details

        template <typename R>
        constexpr bool range = details::range_impl<R>::value;

        template <class T>
        constexpr bool common_range = details::common_range_impl<T>::value;

        // **************************** <ranges> access ************************************************
        template <class>
        constexpr bool disable_sized_range = false;

        namespace details
        {
            struct rbegin_impl
            {
                template <typename R, RAH2_STD::enable_if_t<has_rbegin_member<remove_cvref_t<R>>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.rbegin();
                }

                template <
                    typename R,
                    typename =
                        RAH2_STD::enable_if_t<not ranges::details::has_rbegin_member<remove_cvref_t<R>>>,
                    typename = RAH2_STD::enable_if_t<ranges::details::has_rbegin_ADL<remove_cvref_t<R>>>>
                auto operator()(R&& range) const
                {
                    return rbegin(range);
                }

                template <
                    typename R,
                    typename =
                        RAH2_STD::enable_if_t<not ranges::details::has_rbegin_member<remove_cvref_t<R>>>,
                    typename =
                        RAH2_STD::enable_if_t<not ranges::details::has_rbegin_ADL<remove_cvref_t<R>>>,
                    RAH2_STD::enable_if_t<common_range<R>>* = nullptr,
                    RAH2_STD::enable_if_t<bidirectional_iterator<iterator_t<R>>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return RAH2_STD::make_reverse_iterator(end(range));
                }
            };

            struct rend_impl
            {
                template <typename R, RAH2_STD::enable_if_t<has_rend_member<remove_cvref_t<R>>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.rend();
                }

                template <
                    typename R,
                    typename =
                        RAH2_STD::enable_if_t<not ranges::details::has_rend_member<remove_cvref_t<R>>>,
                    RAH2_STD::enable_if_t<has_rend_ADL<remove_cvref_t<R>>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return rend(range);
                }

                template <
                    typename R,
                    typename =
                        RAH2_STD::enable_if_t<not ranges::details::has_rend_member<remove_cvref_t<R>>>,
                    RAH2_STD::enable_if_t<not ranges::details::has_rend_ADL<remove_cvref_t<R>>>* = nullptr,
                    RAH2_STD::enable_if_t<common_range<R>>* = nullptr,
                    RAH2_STD::enable_if_t<bidirectional_iterator<ranges::iterator_t<R>>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return RAH2_STD::make_reverse_iterator(begin(range));
                }
            };

            struct crbegin_impl
            {
                template <typename R>
                auto operator()(R const& range) const
                {
                    return range.crbegin();
                }
            };
            struct crend_impl
            {
                template <typename R>
                auto operator()(R const& range) const
                {
                    return range.crend();
                }
            };

            struct size_impl
            {
                template <class T, size_t N>
                constexpr size_t operator()(T (&)[N]) const noexcept
                {
                    return N;
                }
                template <class T, size_t N>
                constexpr size_t operator()(T const (&)[N]) const noexcept
                {
                    return N;
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<
                        has_size_member<R> && !disable_sized_range<RAH2_STD::remove_cv_t<R>>>* = nullptr>
                constexpr auto operator()(R&& range) const
                {
                    return range.size();
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<
                        !(has_size_member<R> && !disable_sized_range<RAH2_STD::remove_cv_t<R>>)
                        && has_size_ADL<R>>* = nullptr>
                constexpr auto operator()(R&& range) const
                {
                    return size(range);
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<
                        !has_size_member<R> && !has_size_ADL<R>
                        && sized_sentinel_for<iterator_t<R>, sentinel_t<R>>>* = nullptr>
                constexpr auto operator()(R&& range) const -> decltype(end(range) - begin(range))
                {
                    return end(range) - begin(range);
                }
            };

            struct ssize_impl
            {
                template <typename R>
                std::ptrdiff_t operator()(R&& range) const
                {
                    return static_cast<std::ptrdiff_t>(size_impl{}(RAH2_STD::forward<R>(range)));
                }
            };

            struct empty_impl
            {
                template <typename R, RAH2_STD::enable_if_t<has_empty_member<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.empty();
                }

                template <typename T2>
                using has_ranges_size =
                    decltype(size_impl{}(RAH2_STD::declval<RAH2_STD::remove_reference_t<T2>>()));

                template <
                    typename R,
                    RAH2_STD::enable_if_t<
                        !has_empty_member<R> && concepts::compiles<false, R, has_ranges_size>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return size_impl{}(range) == 0;
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<
                        !has_empty_member<R> && !concepts::compiles<false, R, has_ranges_size>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return begin_impl{}(range) == end_impl{}(range);
                }
            };

            struct data_impl
            {
                template <typename R, RAH2_STD::enable_if_t<has_data_member<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.data();
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<!has_data_member<R> and contiguous_iterator<iterator_t<R>>>* = nullptr>
                auto operator()(R&& range) const
                    -> decltype(&(*ranges::begin(RAH2_STD::forward<R>(range))))
                {
                    return &(*ranges::begin(RAH2_STD::forward<R>(range)));
                }
            };

            struct cdata_impl
            {
                template <typename R>
                auto operator()(R const& range) const
                {
                    return range.cdata();
                }
            };

        } // namespace details

        constexpr auto rbegin = details::rbegin_impl();
        constexpr auto rend = details::rend_impl();
        constexpr auto crbegin = details::crbegin_impl();
        constexpr auto crend = details::crend_impl();
        constexpr auto size = details::size_impl();
        constexpr auto ssize = details::ssize_impl();
        constexpr auto empty = details::empty_impl();
        constexpr auto data = details::data_impl();
        constexpr auto cdata = details::cdata_impl();

        // **************************** <ranges> traits ***********************************************
        namespace details
        {
            template <typename A, typename B>
            using common_iterator_tag = RAH2_STD::conditional_t<RAH2_NS::derived_from<A, B>, B, A>;

            template <typename A, typename B>
            using max_iterator_tag = RAH2_STD::conditional_t<RAH2_NS::derived_from<A, B>, A, B>;

            template <typename Cat, typename MinCat, typename MaxCat>
            using cap_iterator_tag = max_iterator_tag<common_iterator_tag<Cat, MaxCat>, MinCat>;
        } // namespace details

        //template <typename T>
        //using iterator_t = decltype(RAH2_NS::ranges::begin(RAH2_STD::declval<T>()));

        template <typename T>
        using const_iterator_t = decltype(RAH2_NS::ranges::cbegin(RAH2_STD::declval<T>()));

        template <typename T>
        using const_sentinel_t = decltype(RAH2_NS::ranges::cend(RAH2_STD::declval<T>()));

        template <typename R>
        using range_size_t = decltype(RAH2_NS::ranges::size(RAH2_STD::declval<R&>()));

        template <typename R>
        using range_difference_t = RAH2_NS::iter_difference_t<iterator_t<R>>;

        template <typename R>
        using range_value_t = RAH2_NS::iter_value_t<iterator_t<R>>;

        template <typename R>
        using range_reference_t = RAH2_NS::iter_reference_t<iterator_t<R>>;

        template <typename R>
        using range_const_reference_t = RAH2_NS::iter_const_reference_t<iterator_t<R>>;

        template <typename R>
        using range_rvalue_reference_t = RAH2_NS::iter_rvalue_reference_t<iterator_t<R>>;

        namespace details
        {
            template <typename R>
            using range_iter_categ_t = RAH2_NS::details::iterator_concept<iterator_t<R>>;
        }

        // ******************************** <ranges> concepts *****************************************
        template <class R>
        constexpr bool enable_borrowed_range = false;

        template <class R>
        constexpr bool borrowed_range =
            range<R>
            && (RAH2_NS::is_lvalue_reference_v<R> || enable_borrowed_range<remove_cvref_t<R>>);

        namespace details
        {
            template <typename T, bool Diagnostic = false>
            struct sized_range_impl
            {
                template <typename T2>
                using has_ranges_size = decltype(RAH2_NS::ranges::size(
                    RAH2_STD::declval<RAH2_STD::remove_reference_t<T2>>()));

                constexpr static bool value =
                    range_impl<T, Diagnostic>::value
                    && concepts::compiles<Diagnostic, T, has_ranges_size>
                    && concepts::is_true_v<Diagnostic, !(disable_sized_range<T>)>;
            };

            template <class R, class T, bool Diagnostic = false>
            struct output_range_impl
            {
                template <typename U>
                using output_iterator = RAH2_STD::enable_if_t<output_iterator<iterator_t<U>, T>>;

                static constexpr bool value = range_impl<R, Diagnostic>::value
                                              && concepts::compiles<Diagnostic, R, output_iterator>;
            };

            template <class T, bool Diagnostic = false>
            struct input_range_impl
            {
                template <typename U>
                using is_input = RAH2_STD::enable_if_t<
                    RAH2_NS::details::input_iterator_impl<iterator_t<U>, Diagnostic>::value>;

                static constexpr bool value =
                    range_impl<T, Diagnostic>::value && concepts::compiles<Diagnostic, T, is_input>;
            };

            template <class T, bool Diagnostic = false>
            struct forward_range_impl
            {
                template <typename U>
                using forward_iterator = RAH2_STD::enable_if_t<
                    RAH2_NS::details::forward_iterator_impl<iterator_t<U>, Diagnostic>::value>;

                static constexpr bool value = input_range_impl<T, Diagnostic>::value
                                              && concepts::compiles<Diagnostic, T, forward_iterator>;
            };

            template <class T, bool Diagnostic = false>
            struct bidirectional_range_impl
            {
                template <typename U>
                using bidir_iter = RAH2_STD::enable_if_t<
                    RAH2_NS::details::bidirectional_iterator_impl<iterator_t<U>, Diagnostic>::value>;

                static constexpr bool value = forward_range_impl<T, Diagnostic>::value
                                              && concepts::compiles<Diagnostic, T, bidir_iter>;
            };

            template <class T, bool Diagnostic = false>
            struct random_access_range_impl
            {
                template <typename U>
                using has_random_access_iterator = RAH2_STD::enable_if_t<
                    RAH2_NS::details::random_access_iterator_impl<iterator_t<U>, Diagnostic>::value>;

                static constexpr bool value =
                    bidirectional_range_impl<T, Diagnostic>::value
                    && concepts::compiles<Diagnostic, T, has_random_access_iterator>;
            };

            template <typename R, bool Diagnostic = false>
            struct contiguous_range_impl
            {
                template <typename T>
                using has_data = RAH2_STD::enable_if_t<RAH2_NS::is_same_v<
                    decltype(RAH2_NS::ranges::data(RAH2_STD::declval<T>())),
                    RAH2_STD::add_pointer_t<RAH2_STD::remove_reference_t<range_reference_t<T>>>>>;
                template <typename T>
                using contiguous_iterator = RAH2_STD::enable_if_t<
                    RAH2_NS::details::contiguous_iterator_impl<iterator_t<T>, Diagnostic>::value>;

                static constexpr bool value =
                    random_access_range_impl<R, Diagnostic>::value
                    && concepts::compiles<Diagnostic, R, contiguous_iterator>
                    && concepts::compiles<Diagnostic, R, has_data>;
            };

            template <class T, bool Diagnostic = false>
            struct constant_range_impl
            {
                template <class U>
                static constexpr bool constant_iterator =
                    input_iterator<U>
                    && RAH2_NS::is_same_v<iter_const_reference_t<U>, iter_reference_t<U>>;

                template <typename U>
                using const_iter = RAH2_STD::enable_if_t<constant_iterator<iterator_t<U>>>;

                static constexpr bool value = input_range_impl<T, Diagnostic>::value
                                              && concepts::compiles<Diagnostic, T, const_iter>;
            };
        } // namespace details

        template <class T>
        constexpr bool sized_range = details::sized_range_impl<T>::value;

        template <typename V>
        struct view_interface;

        template <typename R>
        constexpr bool enable_view =
            derived_from<R, view_base> || derived_from<R, view_interface<R>>;

        template <typename T>
        constexpr bool view = range<T> && RAH2_NS::movable<T> && RAH2_NS::ranges::enable_view<T>;

        template <class R, class T>
        constexpr bool output_range = details::output_range_impl<R, T>::value;

        template <class T>
        constexpr bool input_range = details::input_range_impl<T>::value;

        template <class T>
        constexpr bool forward_range = details::forward_range_impl<T>::value;

        template <class T>
        constexpr bool bidirectional_range = details::bidirectional_range_impl<T>::value;

        template <class T>
        constexpr bool random_access_range = details::random_access_range_impl<T>::value;

        template <typename R, typename = int>
        struct has_ranges_data
        {
            static constexpr bool value = false;
        };

        template <typename R>
        struct has_ranges_data<R, decltype(data(RAH2_STD::declval<R>()), 0)>
        {
            static constexpr bool value = true;
        };

        template <class R>
        constexpr bool contiguous_range = details::contiguous_range_impl<R>::value;

        template <class T>
        constexpr bool constant_range = details::constant_range_impl<T>::value;

        template <class T>
        constexpr bool viewable_range =
            range<T>
            && ((RAH2_NS::ranges::view<RAH2_NS::remove_cvref_t<T>>
                 && RAH2_NS::constructible_from<RAH2_NS::remove_cvref_t<T>, T>)
                || (!RAH2_NS::ranges::view<RAH2_NS::remove_cvref_t<T>>
                    && (RAH2_NS::is_lvalue_reference_v<T>
                        || (RAH2_NS::movable<RAH2_STD::remove_reference_t<T>>
                            && !is_initializer_list<T>))));

        // ******************************* ranges views ***********************************************
#define RAH2_SELF (*static_cast<T* const>(this))
#define RAH2_SELF_CONST (*static_cast<T const* const>(this))

        template <typename T>
        struct view_interface : view_base
        {
            auto empty()
            {
                return RAH2_SELF.begin() == RAH2_SELF.end();
            }

            auto front() // -> decltype(*(details::template declval<T const>().begin()))
            {
                return *(RAH2_SELF.begin());
            }

            auto back() // -> decltype(*(RAH2_SELF_CONST.end()))
            {
                auto last = RAH2_SELF.end();
                --last;
                return *last;
            }

            template <typename U = T, RAH2_STD::enable_if_t<RAH2_NS::ranges::random_access_range<U>>* = nullptr>
            auto operator[](size_t index) // -> decltype(*(RAH2_SELF_CONST.begin()))
            {
                return *(RAH2_SELF.begin() + index);
            }
        };

        template <typename I, typename S = I>
        class subrange : public view_interface<subrange<I, S>>
        {
            I iterator_;
            S sentinel_;
            using iter_cat = RAH2_NS::details::iterator_concept<I>;

        public:
            subrange() = default;
            subrange(I a, S b)
                : iterator_(RAH2_STD::move(a))
                , sentinel_(RAH2_STD::move(b))
            {
            }

            I begin() const
            {
                return iterator_;
            }
            S end() const
            {
                return sentinel_;
            }
            template <
                typename Cat = iter_cat,
                RAH2_STD::enable_if_t<RAH2_NS::derived_from<Cat, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
            auto data()
            {
                return &(*iterator_);
            }

            template <typename U = I, RAH2_STD::enable_if_t<sized_sentinel_for<S, U>>* = nullptr>
            auto size()
            {
                return sentinel_ - iterator_;
            }

            template <typename U = I, RAH2_STD::enable_if_t<sized_sentinel_for<S const, U const>>* = nullptr>
            auto size() const
            {
                return sentinel_ - iterator_;
            }
        };

        template <class I, class S>
        constexpr bool enable_borrowed_range<subrange<I, S>> = true;

        template <typename I, typename S>
        auto make_subrange(I b, S e)
        {
            return subrange<I, S>{b, e};
        }

        template <class D>
        class closure_object_facade
        {
            template <typename R>
            friend auto operator|(R&& range, D const& closure)
            {
                return closure(RAH2_STD::forward<R>(range));
            }
        };

        // ********************** <ranges> Dangling iterator handling  ****************************
        struct dangling
        {
            constexpr dangling() noexcept = default;
            template <class... Args>
            constexpr explicit dangling(Args&&...) noexcept
            {
            }
        };

        template <typename R>
        using borrowed_iterator_t =
            RAH2_STD::conditional_t<RAH2_NS::ranges::borrowed_range<R>, iterator_t<R>, RAH2_NS::ranges::dangling>;

        template <typename R>
        using borrowed_subrange_t = RAH2_STD::conditional_t<
            RAH2_NS::ranges::borrowed_range<R>,
            RAH2_NS::ranges::subrange<iterator_t<R>>,
            RAH2_NS::ranges::dangling>;

        // ***************************** <iterator> functions *****************************************
        namespace niebloids
        {
            struct distance
            {
                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        !RAH2_NS::sized_sentinel_for<S, I>
                        && RAH2_NS::details::weakly_equality_comparable_with<S, I>>* = nullptr>
                RAH2_NS::iter_difference_t<I> operator()(I first, S last) const
                {
                    iter_difference_t<I> len = 0;
                    for (; first != last; ++first, ++len)
                    {
                    }
                    return len;
                }

                template <typename I, typename S, RAH2_STD::enable_if_t<RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                auto operator()(I first, S last) const
                {
                    return last - first;
                }
                template <typename R>
                auto operator()(R&& r) const
                {
                    return (*this)(begin(r), end(r));
                }
            };
        } // namespace niebloids
        constexpr niebloids::distance distance;

        namespace niebloids
        {
            struct advance
            {
                template <typename I, typename S, RAH2_STD::enable_if_t<RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 intptr_t operator()(I& i, iter_difference_t<I> n, S const& bound) const
                {
                    // RAH2_STD::abs is not constexpr until C++23
                    auto abs = [](intptr_t const x)
                    {
                        return x < 0 ? -x : x;
                    };

                    auto const distToBound = bound - i;
                    if (distToBound == 0)
                    {
                        return n;
                    }

                    if ((n * distToBound) > 0) // Same side
                    {
                        auto const dist = abs(n) - abs(distToBound);
                        if (dist > 0) // bound is lower
                        {
                            // i = RAH2_STD::move(bound);
                            RAH2_STD::advance(i, bound - i);
                            return n - distToBound;
                        }
                    }
                    RAH2_STD::advance(i, n);
                    return 0;
                }
                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<not RAH2_NS::sized_sentinel_for<S, I>>* = nullptr,
                    RAH2_STD::enable_if_t<RAH2_NS::bidirectional_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 intptr_t operator()(I& i, iter_difference_t<I> n, S const& bound) const
                {
                    while (n > 0 && i != bound)
                    {
                        --n;
                        ++i;
                    }

                    while (n < 0 && i != bound)
                    {
                        ++n;
                        --i;
                    }

                    return n;
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<not RAH2_NS::sized_sentinel_for<S, I>>* = nullptr,
                    RAH2_STD::enable_if_t<not RAH2_NS::bidirectional_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 intptr_t operator()(I& i, iter_difference_t<I> n, S const& bound) const
                {
                    RAH2_ASSERT(n >= 0);
                    while (n > 0 && i != bound)
                    {
                        --n;
                        ++i;
                    }
                    return n;
                }

                template <
                    typename I,
                    RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I>>* = nullptr,
                    RAH2_STD::enable_if_t<not RAH2_NS::bidirectional_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 void operator()(I& i, RAH2_NS::iter_difference_t<I> n) const
                {
                    RAH2_ASSERT(n >= 0);
                    while (n > 0)
                    {
                        --n;
                        ++i;
                    }
                }

                template <
                    typename I,
                    RAH2_STD::enable_if_t<RAH2_NS::bidirectional_iterator<I>>* = nullptr,
                    RAH2_STD::enable_if_t<not RAH2_NS::random_access_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 void operator()(I& i, RAH2_NS::iter_difference_t<I> n) const
                {
                    while (n > 0)
                    {
                        --n;
                        ++i;
                    }

                    while (n < 0)
                    {
                        ++n;
                        --i;
                    }
                }

                template <typename I, RAH2_STD::enable_if_t<RAH2_NS::random_access_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 void operator()(I& i, RAH2_NS::iter_difference_t<I> n) const
                {
                    i += n;
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>
                        && RAH2_NS::assignable_from<I&, S>>* = nullptr>
                RAH2_CONSTEXPR20 void operator()(I& i, S bound) const
                {
                    i = RAH2_STD::move(bound);
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>
                        && not RAH2_NS::assignable_from<I&, S> && RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 void operator()(I& i, S bound) const
                {
                    operator()(i, bound - i);
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>
                        && not RAH2_NS::assignable_from<I&, S>
                        && not RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 void operator()(I& i, S bound) const
                {
                    while (i != bound)
                        ++i;
                }
            };
        } // namespace niebloids
        constexpr niebloids::advance advance;

        namespace niebloids
        {
            struct next
            {
                template <typename I, RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I i) const
                {
                    ++i;
                    return i;
                }

                template <typename I, RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I i, RAH2_NS::iter_difference_t<I> n) const
                {
                    RAH2_NS::ranges::advance(i, n);
                    return i;
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I i, S const& bound) const
                {
                    static_assert(RAH2_NS::semiregular<S>, "RAH2_NS::semiregular<S>");
                    static_assert(
                        RAH2_NS::input_or_output_iterator<I>, "RAH2_NS::input_or_output_iterator<I>");
                    static_assert(
                        RAH2_NS::details::weakly_equality_comparable_with<I, S>,
                        "weakly_equality_comparable_with<I, S>");
                    RAH2_NS::ranges::advance(i, bound);
                    return i;
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I i, RAH2_NS::iter_difference_t<I> n, S bound) const
                {
                    RAH2_NS::ranges::advance(i, n, bound);
                    return i;
                }
            };
        } // namespace niebloids
        constexpr niebloids::next next;

    } // namespace ranges

#if !RAH2_INSIDE_EASTL
    template <class C>
    constexpr auto begin(C& c) -> decltype(c.begin())
    {
        return c.begin();
    }
    template <class C>
    constexpr auto begin(C const& c) -> decltype(c.begin())
    {
        return c.begin();
    }

    template <class T, RAH2_STD::size_t N>
    constexpr T* begin(T (&array)[N]) noexcept
    {
        return array;
    }

    template <class C>
    constexpr auto cbegin(const C& c) noexcept -> decltype(RAH2_NS::begin(c))
    {
        return RAH2_NS::begin(c);
    }

    template <class C>
    constexpr auto end(C& c) -> decltype(c.end())
    {
        return c.end();
    }

    template <class C>
    constexpr auto end(const C& c) -> decltype(c.end())
    {
        return c.end();
    }

    template <class T, RAH2_STD::size_t N>
    constexpr T* end(T (&array)[N]) noexcept
    {
        return array + N;
    }

    template <class C>
    constexpr auto cend(const C& c) noexcept -> decltype(RAH2_NS::end(c))
    {
        return RAH2_NS::end(c);
    }

    template <class C>
    constexpr auto rbegin(C& c) -> decltype(c.rbegin())
    {
        return c.rbegin();
    }

    template <class C>
    constexpr auto rbegin(const C& c) -> decltype(c.rbegin())
    {
        return c.rbegin();
    }

    template <class T, RAH2_STD::size_t N>
    constexpr RAH2_STD::reverse_iterator<T*> rbegin(T (&array)[N])
    {
        return RAH2_STD::reverse_iterator<T*>(array + N);
    }

    template <class C>
    constexpr auto crbegin(const C& c) -> decltype(RAH2_NS::rbegin(c))
    {
        return RAH2_NS::rbegin(c);
    }

    template <class C>
    constexpr auto rend(C& c) -> decltype(c.rend())
    {
        return c.rend();
    }

    template <class C>
    constexpr auto rend(const C& c) -> decltype(c.rend())
    {
        return c.rend();
    }

    template <class T, RAH2_STD::size_t N>
    constexpr RAH2_STD::reverse_iterator<T*> rend(T (&array)[N])
    {
        return RAH2_STD::reverse_iterator<T*>(array);
    }

    template <class C>
    constexpr auto crend(const C& c) -> decltype(RAH2_NS::rend(c))
    {
        return RAH2_NS::rend(c);
    }

    template <class C>
    constexpr auto size(const C& c) -> decltype(c.size())
    {
        return c.size();
    }

    template <class C>
    constexpr auto ssize(const C& c)
        -> RAH2_STD::common_type_t<std::ptrdiff_t, RAH2_STD::make_signed_t<decltype(c.size())>>
    {
        return RAH2_STD::common_type_t<std::ptrdiff_t, RAH2_STD::make_signed_t<decltype(c.size())>>(
            c.size());
    }

    template <class T, RAH2_STD::size_t N>
    constexpr RAH2_STD::size_t size(const T (&array)[N]) noexcept
    {
        (void)array;
        return N;
    }

    template <class T, std::ptrdiff_t N>
    constexpr std::ptrdiff_t ssize(const T (&array)[N]) noexcept
    {
        (void)array;
        return static_cast<std::ptrdiff_t>(N);
    }

    template <class C>
    RAH2_NODISCARD constexpr auto empty(const C& c) -> decltype(c.empty())
    {
        return c.empty();
    }

    template <class T, RAH2_STD::size_t N>
    RAH2_NODISCARD constexpr bool empty(const T (&array)[N]) noexcept
    {
        (void)array;
        return N == 0;
    }

    template <class C>
    constexpr auto data(C& c) -> decltype(c.data())
    {
        return c.data();
    }

    template <class C>
    constexpr auto data(const C& c) -> decltype(c.data())
    {
        return c.data();
    }

    template <class T, RAH2_STD::size_t N>
    constexpr T* data(T (&array)[N]) noexcept
    {
        return array;
    }
#endif

    // ********************** <iterator> Classes **************************************************
    struct default_sentinel_t
    {
    };
    constexpr default_sentinel_t default_sentinel{};

    struct unreachable_sentinel_t
    {
        template <typename I>
        friend constexpr bool operator==(unreachable_sentinel_t, I const&) noexcept
        {
            return false;
        }
    };
    constexpr unreachable_sentinel_t unreachable_sentinel{};

#if !RAH2_INSIDE_EASTL
    /// back_insert_iterator
    ///
    /// A back_insert_iterator is simply a class that acts like an iterator but when you
    /// assign a value to it, it calls push_back on the container with the value.
    ///
    template <typename Container>
    class back_insert_iterator
    {
    public:
        typedef back_insert_iterator<Container> this_type;
        typedef Container container_type;
        typedef typename Container::const_reference const_reference;
        typedef RAH2_NS::output_iterator_tag iterator_category;
        typedef void value_type;
        typedef void difference_type;
        typedef void pointer;
        typedef void reference;

    protected:
        Container* container = nullptr;

    public:
        back_insert_iterator() = delete; // Not valid. Must construct with a Container.

        back_insert_iterator(this_type const& x) = delete;
        back_insert_iterator& operator=(this_type const& x) = delete;
        back_insert_iterator(this_type&& x) = default;
        back_insert_iterator& operator=(this_type&& x) = default;

        explicit back_insert_iterator(Container& x)
            : container(&x)
        {
        }

        back_insert_iterator& operator=(const_reference value)
        {
            container->push_back(value);
            return *this;
        }

        back_insert_iterator& operator=(typename Container::value_type&& value)
        {
            container->push_back(RAH2_STD::move(value));
            return *this;
        }

        back_insert_iterator& operator*()
        {
            return *this;
        }

        back_insert_iterator const& operator*() const
        {
            return *this;
        }

        back_insert_iterator& operator++()
        {
            return *this;
        } // This is by design.

        back_insert_iterator operator++(int)
        {
            return *this;
        } // This is by design.
    };

    /// back_inserter
    ///
    /// Creates an instance of a back_insert_iterator.
    ///
    template <typename Container>
    back_insert_iterator<Container> back_inserter(Container& x)
    {
        return back_insert_iterator<Container>(x);
    }
#endif
    // ***************************** algorithm replacement ****************************************
    namespace details
    {
        template <class T>
        constexpr T const& min(T const& a, T const& b)
        {
            return a < b ? a : b;
        }
        template <class T>
        constexpr T const& max(T const& a, T const& b)
        {
            return a > b ? a : b;
        }
    } // namespace details
    // ************************************** movable_box ********************************************
    namespace details
    {

        // Small optional but move copyable event if T is only move constructible
        template <typename T>
        struct movable_box
        {
            movable_box() = default;
            movable_box(movable_box const& other)
            {
                if (other.has_value())
                {
                    new (get_ptr()) T(other.value());
                    is_allocated_ = true;
                }
            }
            movable_box(movable_box&& other) noexcept
            {
                (*this) = RAH2_STD::move(other);
            }
            movable_box& operator=(movable_box const& other)
            {
                if (has_value())
                {
                    if (other.has_value())
                    {
                        // Handle the case where T is not copy assignable
                        reset();
                        new (get_ptr()) T(other.value());
                        is_allocated_ = true;
                    }
                    else
                        reset();
                }
                else
                {
                    if (other.has_value())
                    {
                        new (get_ptr()) T(other.value());
                        is_allocated_ = true;
                    }
                }
                return *this;
            }
            movable_box& operator=(movable_box&& other) noexcept
            {
                if (has_value())
                {
                    if (other.has_value())
                    {
                        // TODO : move copy when possible
                        // A lambda with const capture is not move assignable
                        reset();
                        new (get_ptr()) T(RAH2_STD::move(other.value()));
                        is_allocated_ = true;
                    }
                    else
                        reset();
                }
                else
                {
                    if (other.has_value())
                    {
                        new (get_ptr()) T(RAH2_STD::move(other.value()));
                        is_allocated_ = true;
                    }
                }
                return *this;
            }
            movable_box(T const& other)
                : is_allocated_(true)
            {
                new (get_ptr()) T(other);
            }
            movable_box(T&& other)
                : is_allocated_(true)
            {
                new (get_ptr()) T(RAH2_STD::move(other));
            }
            template <typename... Args>
            explicit movable_box(RAH2_NS::in_place_t, Args&&... args)
                : is_allocated_(true)
            {
                new (get_ptr()) T(RAH2_STD::forward<Args>(args)...);
            }
            movable_box& operator=(T const& other)
            {
                reset();
                new (get_ptr()) T(other);
                is_allocated_ = true;
                return *this;
            }
            movable_box& operator=(T&& other)
            {
                reset();
                new (get_ptr()) T(RAH2_STD::move(other));
                is_allocated_ = true;
                return *this;
            }
            ~movable_box()
            {
                reset();
            }

            friend bool operator==(movable_box const& box, std::nullptr_t)
            {
                return !box.is_allocated_;
            }
            friend bool operator==(std::nullptr_t, movable_box const& box)
            {
                return !box.is_allocated_;
            }

            bool has_value() const
            {
                return is_allocated_;
            }

            void reset()
            {
                if (is_allocated_)
                {
                    destruct_value();
                    is_allocated_ = false;
                }
            }

            T& value()
            {
                RAH2_ASSERT(is_allocated_);
                return *get_ptr();
            }

            T const& value() const
            {
                RAH2_ASSERT(is_allocated_);
                return *get_ptr();
            }

            T& operator*()
            {
                return value();
            }
            T const& operator*() const
            {
                return value();
            }
            T* operator->()
            {
                RAH2_ASSERT(is_allocated_);
                return get_ptr();
            }
            T const* operator->() const
            {
                RAH2_ASSERT(is_allocated_);
                return get_ptr();
            }

        private:
            T* get_ptr()
            {
                return reinterpret_cast<T*>(value_);
            }
            T const* get_ptr() const
            {
                return reinterpret_cast<T const*>(value_);
            }
            void destruct_value()
            {
                value().~T();
            }
            alignas(RAH2_NS::alignment_of_v<T>) unsigned char value_[sizeof(T)];
            bool is_allocated_ = false;
        };
    } // namespace details
} // namespace RAH2_NS
