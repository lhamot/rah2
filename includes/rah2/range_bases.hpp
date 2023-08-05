#pragma once

#include "concepts.hpp"

#include <cassert>
#include <iterator>
#include <utility> // std::move

#define RAH2_ITC_NS RAH2_NS
#define RAHAllocatorType RAH2_STD::allocator
#define RAH2_ASSERT(expression) assert(expression)

#define RAH2_VALIDATE_COMPARE(CHECK)

#define RAH2_DEV_ASSERT assert

#define RAH2_INVOKE_0(FUNC) FUNC()
#define RAH2_INVOKE_1(FUNC, ARG) FUNC(ARG)
#define RAH2_INVOKE_2(FUNC, ARG1, ARG2) FUNC(ARG1, ARG2)

#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 202000L) || __cplusplus >= 202000L)
#define RAH2_CPP20 1
#else
#define RAH2_CPP20 0
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
#else
#define RAH2_NODISCARD
#endif

#if !RAH2_CPP20
#include <ciso646>
#endif

namespace RAH2_NS
{
    // ***************************** <type_traits> traits *****************************************
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

    struct view_base
    {
    };

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
    template <typename T>
    constexpr bool is_initializer_list = is_initializer_list_impl<remove_cvref_t<T>>::value;

    // ****************************** <utility> helpers *******************************************
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

    // **************************** <functional> Classes ******************************************
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
            return t == u;
        }
    };

    struct identity
    {
        template <class T>
        constexpr T&& operator()(T&& t) const noexcept
        {
            // Faster than forward in debug mode
            return static_cast<T&&>(t);
        }
    };

    // ***************************** <concepts> concept *******************************************
    template <class Derived, class Base>
    constexpr bool derived_from =
        RAH2_NS::is_base_of_v<Base, Derived>
        && RAH2_NS::is_convertible_v<Derived const volatile*, Base const volatile*>;

    template <class T, class... Args>
    constexpr bool constructible_from =
        RAH2_NS::is_destructible_v<T> && RAH2_NS::is_constructible_v<T, Args...>;

    template <class From, class To, bool Diagnostic = false>
    struct convertible_to_impl
    {
        template <class F>
        using can_static_cast = decltype(static_cast<To>(RAH2_STD::declval<F>()));

        static constexpr bool value = is_true_v<Diagnostic, is_convertible_v<From, To>>
                                      && compiles<Diagnostic, From, can_static_cast>;
    };

    template <class From, class To>
    constexpr bool convertible_to = convertible_to_impl<From, To>::value;

    template <class T>
    constexpr bool move_constructible =
        RAH2_NS::constructible_from<T, T> && RAH2_NS::convertible_to<T, T>;

    template <class U, typename V>
    constexpr bool same_as = RAH2_NS::is_same_v<U, V>;

    template <class LHS, class RHS, bool Diagnostic = false>
    struct assignable_from_impl
    {
        template <class LHS_>
        using check_assign = RAH2_STD::enable_if_t<RAH2_NS::is_same_v<
            decltype(RAH2_STD::declval<LHS_>() = RAH2_STD::forward<RHS>(RAH2_STD::declval<RHS&&>())),
            LHS_>>;

        static constexpr bool value = is_true_v<Diagnostic, RAH2_NS::is_lvalue_reference_v<LHS>>
                                      && compiles<Diagnostic, LHS, check_assign>;
    };

    template <class LHS, class RHS>
    constexpr bool assignable_from = assignable_from_impl<LHS, RHS>::value;

    MAKE_CONCEPT(swappable, RAH2_STD::swap(RAH2_STD::declval<T&>(), RAH2_STD::declval<T&>()));

    template <class T>
    constexpr bool is_object_v = RAH2_STD::is_object<T>::value;

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

    template <typename T, bool Diagnostic = false>
    struct default_initializable_impl
    {
        template <typename T2>
        using default_ctor = decltype(T2{});

        constexpr static bool value = is_true_v<Diagnostic, RAH2_NS::constructible_from<T>>
                                      && compiles<Diagnostic, T, default_ctor>;
    };
    template <typename T>
    constexpr bool default_initializable = default_initializable_impl<T>::value;

    template <class T, bool Diagnostic = false>
    constexpr bool semiregular = is_true_v<Diagnostic, RAH2_NS::copyable<T>>
                                 && RAH2_NS::default_initializable_impl<T, Diagnostic>::value;

    namespace details
    {
        template <class T, class U, bool Diagnostic = false>
        struct weakly_equality_comparable_with_impl
        {
            template <class T2>
            using check =
                decltype(!(RAH2_STD::declval<T2>() == RAH2_STD::declval<U>()), !(RAH2_STD::declval<T2>() != RAH2_STD::declval<U>()), !(RAH2_STD::declval<U>() == RAH2_STD::declval<T2>()), !(RAH2_STD::declval<U>() != RAH2_STD::declval<T2>()));

            constexpr static bool value = compiles<Diagnostic, T, check>;
        };
        template <class T, class U, bool Diagnostic = false>
        constexpr static bool weakly_equality_comparable_with =
            weakly_equality_comparable_with_impl<T, U, Diagnostic>::value;
    } // namespace details

    template <class T, bool Diagnostic = false>
    constexpr bool equality_comparable = details::weakly_equality_comparable_with<T, T, Diagnostic>;

    template <class T, bool Diagnostic = false>
    constexpr bool regular = semiregular<T, Diagnostic> && equality_comparable<T, Diagnostic>;

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

            static constexpr bool value = compiles2<Diagnostic, NoRefT, NoRefU, t_lesser_u>
                                          && compiles2<Diagnostic, NoRefT, NoRefU, t_greater_u>
                                          && compiles2<Diagnostic, NoRefT, NoRefU, t_lesserequal_u>
                                          && compiles2<Diagnostic, NoRefT, NoRefU, t_greaterequal_u>
                                          && compiles2<Diagnostic, NoRefU, NoRefT, t_lesser_u>
                                          && compiles2<Diagnostic, NoRefU, NoRefT, t_greater_u>
                                          && compiles2<Diagnostic, NoRefU, NoRefT, t_lesserequal_u>
                                          && compiles2<Diagnostic, NoRefU, NoRefT, t_greaterequal_u>;
        };
        template <typename T, typename U>
        constexpr bool partially_ordered_with = partially_ordered_with_impl<T, U>::value;

    } // namespace details

    template <class T, bool Diagnostic = false>
    struct totally_ordered_impl
    {
        static constexpr bool value =
            is_true_v<Diagnostic, RAH2_NS::equality_comparable<T>>
            && details::partially_ordered_with_impl<T, T, Diagnostic>::value;
    };

    template <class T>
    constexpr bool totally_ordered = totally_ordered_impl<T>::value;

    // **************************** <iterator> traits *********************************************
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
        template <class T>
        constexpr auto iter_move(T&& t) -> RAH2_STD::remove_reference_t<decltype(*t)>
        {
            return RAH2_STD::move(*t);
        }

        template <class ForwardIterator1, class ForwardIterator2>
        constexpr void iter_swap(ForwardIterator1 left, ForwardIterator2 right)
        {
            RAH2_STD::swap(*left, *right);
        }
    } // namespace ranges

    // **************************** <iterator> concepts *******************************************
    namespace details
    {
        template <typename I>
        using iterator_category = std::conditional_t<
            RAH2_NS::is_pointer_v<I>,
            RAH2_NS::contiguous_iterator_tag,
            typename RAH2_STD::iterator_traits<I>::iterator_category>;

    } // namespace details

    template <class Out, class T, bool Diagnostic = false>
    struct indirectly_writable_impl
    {
        template <typename O>
        using can_indirect_assign1 = decltype(*RAH2_STD::declval<O&&>() = RAH2_STD::declval<T>());
        template <typename O>
        using can_indirect_assign2 = decltype(*RAH2_STD::declval<O>() = RAH2_STD::declval<T>());

        static constexpr bool value = compiles<Diagnostic, Out, can_indirect_assign1>
                                      && compiles<Diagnostic, Out, can_indirect_assign2>;
    };

    template <class Out, class T>
    constexpr bool indirectly_writable = indirectly_writable_impl<Out, T>::value;

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
            // compiles<In, has_value> &&
            // compiles<In, has_ref> &&
            // compiles<In, has_rval_ref> &&
            compiles<Diagnostic, In, deref>
            // && compiles<In, itermove>
            ;
    };

    template <typename T>
    constexpr bool indirectly_readable = indirectly_readable_impl<remove_cvref_t<T>>::value;

    template <class In, class Out, bool Diagnostic = false>
    struct indirectly_movable_impl
    {
        template <typename In2>
        using indirectly_writable =
            RAH2_STD::enable_if_t<RAH2_NS::indirectly_writable<Out, RAH2_NS::iter_rvalue_reference_t<In2>>>;

        static constexpr bool value = is_true_v<Diagnostic, RAH2_NS::indirectly_readable<In>>
                                      && compiles<Diagnostic, In, indirectly_writable>;
    };

    template <class In, class Out>
    constexpr bool indirectly_movable = indirectly_movable_impl<In, Out>::value;

    template <class In, class Out, bool Diagnostic = false>
    struct indirectly_movable_storable_impl
    {
        template <typename In2>
        using indirectly_writable =
            RAH2_STD::enable_if_t<RAH2_NS::indirectly_writable<Out, RAH2_NS::iter_value_t<In2>>>;
        template <typename In2>
        using movable = RAH2_STD::enable_if_t<RAH2_NS::movable<RAH2_NS::iter_value_t<In2>>>;
        template <typename In2>
        using constructible_from = RAH2_STD::enable_if_t<
            RAH2_NS::constructible_from<RAH2_NS::iter_value_t<In2>, RAH2_NS::iter_rvalue_reference_t<In2>>>;
        template <typename In2>
        using assignable_from = RAH2_STD::enable_if_t<
            RAH2_NS::assignable_from<RAH2_NS::iter_value_t<In2>&, RAH2_NS::iter_rvalue_reference_t<In2>>>;

        static constexpr bool value = RAH2_NS::indirectly_movable_impl<In, Out, Diagnostic>::value
                                      && compiles<Diagnostic, In, indirectly_writable>
                                      && compiles<Diagnostic, In, movable>
                                      && compiles<Diagnostic, In, constructible_from>
                                      && compiles<Diagnostic, In, assignable_from>;
    };

    template <class In, class Out>
    constexpr bool indirectly_movable_storable = indirectly_movable_storable_impl<In, Out>::value;

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
            is_true_v<Diagnostic, RAH2_NS::indirectly_readable<I1>>
            && is_true_v<Diagnostic, RAH2_NS::indirectly_readable<I2>>
            && compiles<Diagnostic, I1, can_swap_1_1> && compiles<Diagnostic, I1, can_swap_1_2>
            && compiles<Diagnostic, I1, can_swap_2_1> && compiles<Diagnostic, I2, can_swap_2_2>;
    };
    template <class I1, class I2 = I1>
    constexpr bool indirectly_swappable = indirectly_swappable_impl<I1, I2>::value;

    template <typename I, bool Diagnostic = false>
    struct weakly_incrementable_impl
    {
        template <class T>
        using diff_is_signed_integer = RAH2_STD::enable_if_t<
            RAH2_NS::is_integral_v<iter_difference_t<T>> && RAH2_NS::is_signed_v<iter_difference_t<T>>>;
        template <class T>
        using incr_type =
            RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(++RAH2_STD::declval<T>()), T&>>;

        template <class T>
        using can_post_incr = decltype(RAH2_STD::declval<T&>()++);

        static constexpr bool value = is_true_v<Diagnostic, movable<I>>
                                      && compiles<Diagnostic, I, diff_is_signed_integer>
                                      && compiles<Diagnostic, I, can_post_incr>;
    };

    template <typename T>
    constexpr bool weakly_incrementable = weakly_incrementable_impl<T>::value;

    template <typename I, bool Diagnostic = false>
    struct incrementable_impl
    {
        template <typename U>
        using check_incr =
            RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U&>()++), U>>;

        static constexpr bool value = regular<I, Diagnostic>
                                      && weakly_incrementable_impl<I, Diagnostic>::value
                                      && compiles<Diagnostic, I, check_incr>;
    };
    template <typename I>
    constexpr bool incrementable = incrementable_impl<I>::value;

    template <typename R, bool Diagnostic = false>
    struct input_or_output_iterator_impl
    {
        template <class T>
        using can_ref =
            RAH2_STD::enable_if_t<not RAH2_NS::is_same_v<decltype(*RAH2_STD::declval<T>()), void>>;

        static constexpr bool value =
            weakly_incrementable_impl<R, Diagnostic>::value && compiles<Diagnostic, R, can_ref>;
    };

    template <typename T>
    constexpr bool input_or_output_iterator = input_or_output_iterator_impl<T>::value;

    template <class I, class T, bool Diagnostic = false>
    struct output_iterator_impl
    {
        template <typename U>
        using can_assign_incr = decltype(*RAH2_STD::declval<U&>()++ = RAH2_STD::declval<T&&>());

        static constexpr bool value = is_true_v<Diagnostic, RAH2_NS::input_or_output_iterator<I>>
                                      && RAH2_NS::indirectly_writable_impl<I, T, Diagnostic>::value
                                      && RAH2_NS::compiles<Diagnostic, I, can_assign_incr>;
    };

    template <class I, class T>
    constexpr bool output_iterator = output_iterator_impl<I, T>::value;

    template <typename I, bool Diagnostic = false>
    struct input_iterator_impl
    {
        template <typename I2>
        using derived_from_input = RAH2_STD::enable_if_t<
            RAH2_NS::derived_from<details::iterator_category<I2>, RAH2_STD::input_iterator_tag>>;

        static constexpr bool value = is_true_v<Diagnostic, input_or_output_iterator<I>>
                                      && indirectly_readable_impl<I, Diagnostic>::value
                                      && compiles<Diagnostic, I, details::iterator_category>
                                      && compiles<Diagnostic, I, derived_from_input>;
    };

    template <typename T>
    constexpr bool input_iterator = input_iterator_impl<T>::value;

    template <class S, class I, bool Diagnostic = false>
    struct sentinel_for_impl
    {
        static constexpr bool value =
            is_true_v<Diagnostic, RAH2_NS::semiregular<S>>
            && is_true_v<Diagnostic, RAH2_NS::input_or_output_iterator<I>>
            && details::weakly_equality_comparable_with_impl<I, S, Diagnostic>::value;
    };

    template <class S, class I>
    constexpr bool sentinel_for = sentinel_for_impl<S, I>::value;

    template <typename I, bool Diagnostic = false>
    struct forward_iterator_impl
    {
        template <typename U>
        using has_forward_tag = RAH2_STD::enable_if_t<
            derived_from<details::iterator_category<U>, RAH2_STD::forward_iterator_tag>>;

        static constexpr bool value = input_iterator_impl<I, Diagnostic>::value
                                      && compiles<Diagnostic, I, has_forward_tag>
                                      && incrementable_impl<I, Diagnostic>::value
                                      && sentinel_for_impl<I, I, Diagnostic>::value;
    };

    template <typename I>
    constexpr bool forward_iterator = forward_iterator_impl<I>::value;

    template <class I>
    constexpr bool permutable =
        RAH2_NS::forward_iterator<I> && RAH2_NS::indirectly_movable_storable<I, I>
        && RAH2_NS::indirectly_swappable<I, I>;

    template <class S, class I, bool Diagnostic = false>
    static constexpr bool disable_sized_sentinel_for = false;

    template <class S, class I, bool Diagnostic = false>
    struct sized_sentinel_for_impl
    {
        template <class S2>
        using less_s_i =
            decltype(RAH2_STD::declval<S2>() - RAH2_STD::declval<I>(), RAH2_STD::declval<I>() - RAH2_STD::declval<S2>());
        static constexpr bool value =
            RAH2_NS::sentinel_for_impl<S, I, Diagnostic>::value
            && is_true_v<
                Diagnostic,
                !RAH2_NS::disable_sized_sentinel_for<RAH2_STD::remove_cv_t<S>, RAH2_STD::remove_cv_t<I>>>
            && compiles<Diagnostic, S, less_s_i>;
    };
    template <class S, class I>
    static constexpr bool sized_sentinel_for = sized_sentinel_for_impl<S, I>::value;

    template <typename I, bool Diagnostic = false>
    struct bidirectional_iterator_impl
    {
        template <typename U>
        using can_pre_decr =
            std::enable_if_t<RAH2_NS::is_same_v<decltype(--RAH2_STD::declval<U&>()), U&>>;
        template <typename U>
        using can_post_decr =
            std::enable_if_t<RAH2_NS::is_same_v<decltype(RAH2_STD::declval<U&>()--), U>>;
        template <typename U>
        using has_bidir_cat = RAH2_STD::enable_if_t<
            derived_from<details::iterator_category<U>, RAH2_STD::bidirectional_iterator_tag>>;

        static constexpr bool value =
            forward_iterator_impl<I, Diagnostic>::value && compiles<Diagnostic, I, has_bidir_cat>
            && compiles<Diagnostic, I, can_pre_decr> && compiles<Diagnostic, I, can_post_decr>;
    };
    template <typename I>
    constexpr bool bidirectional_iterator = bidirectional_iterator_impl<remove_cvref_t<I>>::value;

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
            RAH2_NS::derived_from<details::iterator_category<U>, RAH2_STD::random_access_iterator_tag>>;

        static constexpr bool value =
            RAH2_NS::bidirectional_iterator_impl<I, Diagnostic>::value
            && compiles<Diagnostic, I, has_random_access_cat>
            && RAH2_NS::totally_ordered_impl<I, Diagnostic>::value
            && RAH2_NS::sized_sentinel_for_impl<I, I, Diagnostic>::value
            && compiles<Diagnostic, I, addEqual> && compiles<Diagnostic, I, add>
            && compiles<Diagnostic, I, add2> && compiles<Diagnostic, I, subEqual>
            && compiles<Diagnostic, I, sub> && compiles<Diagnostic, I, arr>;
    };
    template <typename I>
    constexpr bool random_access_iterator = random_access_iterator_impl<remove_cvref_t<I>>::value;

    template <typename I, bool Diagnostic = false>
    struct contiguous_iterator_impl
    {
        static constexpr bool value =
            RAH2_NS::random_access_iterator_impl<I, Diagnostic>::value
            && is_true_v<Diagnostic, RAH2_NS::derived_from<details::iterator_category<I>, RAH2_NS::contiguous_iterator_tag>>
            && is_true_v<Diagnostic, RAH2_NS::is_lvalue_reference_v<RAH2_NS::iter_reference_t<I>>>
            && is_true_v<
                Diagnostic,
                RAH2_NS::same_as<RAH2_NS::iter_value_t<I>, RAH2_NS::remove_cvref_t<RAH2_NS::iter_reference_t<I>>>>;
    };
    template <typename I>
    static constexpr bool contiguous_iterator = contiguous_iterator_impl<I>::value;

    template <class In, class Out>
    constexpr bool indirectly_copyable =
        RAH2_NS::indirectly_readable<In>
        && RAH2_NS::indirectly_writable<Out, RAH2_NS::iter_reference_t<In>>;

    // **************************** <ranges> access ***********************************************
    namespace ranges
    {
        namespace details
        {
            MAKE_CONCEPT(has_begin_member, (RAH2_STD::declval<T>().begin()));
            MAKE_CONCEPT(has_begin_ADL, (begin(RAH2_STD::declval<T>())));
            MAKE_CONCEPT(has_end_member, (RAH2_STD::declval<T>().end()));
            MAKE_CONCEPT(has_end_ADL, (end(RAH2_STD::declval<T>())));
            MAKE_CONCEPT(
                has_rbegin_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().rbegin())
            MAKE_CONCEPT(has_rbegin_ADL, rbegin(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            MAKE_CONCEPT(has_rend_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().rend())
            MAKE_CONCEPT(has_rend_ADL, rend(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            MAKE_CONCEPT(has_size_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().size())
            MAKE_CONCEPT(has_size_ADL, size(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))
            MAKE_CONCEPT(has_data_member, RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>().data())
            MAKE_CONCEPT(has_data_ADL, data(RAH2_STD::declval<RAH2_STD::remove_reference_t<T>>()))

            struct begin_impl
            {
                template <class T, size_t N>
                T* operator()(T (&array)[N]) const
                {
                    return array;
                }

                template <class T, size_t N>
                T const* operator()(T const (&array)[N]) const
                {
                    return array;
                }

                template <typename R, RAH2_STD::enable_if_t<has_begin_member<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.begin();
                }

                template <typename R, RAH2_STD::enable_if_t<not has_begin_member<R> && has_begin_ADL<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return begin(range);
                }
            };

            struct end_impl
            {
                template <class T, size_t N>
                T* operator()(T (&array)[N]) const noexcept
                {
                    return array + N;
                }

                template <class T, size_t N>
                T const* operator()(T const (&array)[N]) const noexcept
                {
                    return array + N;
                }

                template <typename R, RAH2_STD::enable_if_t<has_end_member<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.end();
                }

                template <typename R, RAH2_STD::enable_if_t<not has_end_member<R> and has_end_ADL<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return end(range);
                }
            };

            struct cbegin_impl
            {
                template <class T, size_t N>
                T const* operator()(T const (&array)[N]) const
                {
                    return array;
                }

                template <typename R>
                auto operator()(R const& range) const
                {
                    return range.begin();
                }
            };

            struct cend_impl
            {
                template <class T, size_t N>
                T const* operator()(T const (&array)[N]) const noexcept
                {
                    return array + N;
                }

                template <typename R>
                auto operator()(R const& range) const
                {
                    return range.end();
                }
            };
        } // namespace details

        inline namespace customization_point_objects
        {
            constexpr auto begin = details::begin_impl();
            constexpr auto end = details::end_impl();
            constexpr auto cbegin = details::cbegin_impl();
            constexpr auto cend = details::cend_impl();

        } // namespace customization_point_objects

        // **************************** <ranges> traits ***********************************************
        template <typename T>
        using iterator_t = decltype(begin(RAH2_STD::declval<T>()));

        template <typename T>
        using sentinel_t = decltype(end(RAH2_STD::declval<T&>()));

        // **************************** <ranges> concepts *********************************************
        template <typename R, bool Diagnostic = false>
        struct range_impl
        {
            template <typename R2>
            using has_begin = decltype(begin(RAH2_STD::declval<R2>()));
            template <typename R2>
            using has_end = decltype(end(RAH2_STD::declval<R2>()));

            static constexpr bool value =
                compiles<Diagnostic, R, has_begin> && compiles<Diagnostic, R, has_end>;
        };

        template <typename R>
        constexpr bool range = range_impl<R>::value;

        template <class T, bool Diagnostic = false>
        struct common_range_impl
        {
            template <typename U>
            using iter_is_sent =
                RAH2_STD::enable_if_t<RAH2_NS::is_same_v<iterator_t<U>, sentinel_t<U>>>;

            static constexpr bool value =
                range_impl<T, Diagnostic>::value && compiles<Diagnostic, T, iter_is_sent>;
        };

        template <class T>
        constexpr bool common_range = common_range_impl<T>::value;

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
                    RAH2_STD::enable_if_t<!(
                        has_size_member<R>
                        && !disable_sized_range<RAH2_STD::remove_cv_t<R>>)&&has_size_ADL<R>>* = nullptr>
                constexpr auto operator()(R&& range) const
                {
                    return size(range);
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<
                        !has_size_member<R> && !has_size_ADL<R>
                        && sized_sentinel_for<iterator_t<R>, sentinel_t<R>>>* = nullptr>
                constexpr auto operator()(R&& range) const
                {
                    return end(range) - begin(range);
                }
            };

            struct ssize_impl
            {
                template <typename R>
                std::ptrdiff_t operator()(R const& range) const
                {
                    return static_cast<std::ptrdiff_t>(size_impl{}(range));
                }
            };

            struct empty_impl
            {
                template <typename R>
                auto operator()(R const& range) const
                {
                    return range.empty();
                }
            };

            struct data_impl
            {
                template <typename R, RAH2_STD::enable_if_t<has_data_member<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return range.data();
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

        inline namespace customization_point_objects
        {
            constexpr auto rbegin = details::rbegin_impl();
            constexpr auto rend = details::rend_impl();
            constexpr auto crbegin = details::crbegin_impl();
            constexpr auto crend = details::crend_impl();
            constexpr auto size = details::size_impl();
            constexpr auto ssize = details::ssize_impl();
            constexpr auto empty = details::empty_impl();
            constexpr auto data = details::data_impl();
            constexpr auto cdata = details::cdata_impl();
        } // namespace customization_point_objects

        // **************************** <ranges> traits ***********************************************
        template <typename A, typename B>
        using common_iterator_tag = RAH2_STD::conditional_t<RAH2_NS::derived_from<A, B>, B, A>;

        template <typename A, typename B>
        using max_iterator_tag = RAH2_STD::conditional_t<RAH2_NS::derived_from<A, B>, A, B>;

        template <typename Cat, typename MinCat, typename MaxCat>
        using cap_iterator_tag = max_iterator_tag<common_iterator_tag<Cat, MaxCat>, MinCat>;

        //template <typename T>
        //using iterator_t = decltype(RAH2_NS::ranges::begin(RAH2_STD::declval<T>()));

        template <typename T>
        using const_iterator_t = decltype(cbegin(RAH2_STD::declval<T>()));

        template <typename T>
        using const_sentinel_t = decltype(cend(RAH2_STD::declval<T>()));

        template <typename R>
        using range_size_t = decltype(size(RAH2_STD::declval<R&>()));

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

        template <typename R>
        using range_iter_categ_t =
            typename RAH2_STD::iterator_traits<iterator_t<R>>::iterator_category;

        // ******************************** <ranges> concepts *****************************************
        template <class R>
        constexpr bool enable_borrowed_range = false;

        template <class R>
        constexpr bool borrowed_range =
            range<R>
            && (RAH2_NS::is_lvalue_reference_v<R> || enable_borrowed_range<remove_cvref_t<R>>);

        template <typename T, bool Diagnostic = false>
        struct sized_range_impl
        {
            template <typename T2>
            using has_ranges_size =
                decltype(RAH2_NS::ranges::size(RAH2_STD::declval<RAH2_STD::remove_reference_t<T2>>()));

            constexpr static bool value = range_impl<T, Diagnostic>::value
                                          && compiles<Diagnostic, T, has_ranges_size>
                                          && is_true_v<Diagnostic, !(disable_sized_range<T>)>;
        };

        template <class T>
        constexpr bool sized_range = sized_range_impl<T>::value;

        template <typename V>
        struct view_interface;

        template <typename R>
        constexpr bool enable_view =
            derived_from<R, view_base> || derived_from<R, view_interface<R>>;

        template <typename T>
        constexpr bool view = range<T> && RAH2_NS::movable<T> && RAH2_NS::ranges::enable_view<T>;

        template <class R, class T, bool Diagnostic = false>
        struct output_range_impl
        {
            template <typename U>
            using output_iterator = RAH2_STD::enable_if_t<output_iterator<iterator_t<U>, T>>;

            static constexpr bool value =
                range_impl<R, Diagnostic>::value && compiles<Diagnostic, R, output_iterator>;
        };

        template <class R, class T>
        constexpr bool output_range = output_range_impl<R, T>::value;

        template <class T, bool Diagnostic = false>
        struct input_range_impl
        {
            template <typename U>
            using is_input =
                RAH2_STD::enable_if_t<input_iterator_impl<iterator_t<U>, Diagnostic>::value>;

            static constexpr bool value =
                range_impl<T, Diagnostic>::value && compiles<Diagnostic, T, is_input>;
        };

        template <class T>
        constexpr bool input_range = input_range_impl<T>::value;

        template <class T, bool Diagnostic = false>
        struct forward_range_impl
        {
            template <typename U>
            using forward_iterator =
                RAH2_STD::enable_if_t<forward_iterator_impl<iterator_t<U>, Diagnostic>::value>;

            static constexpr bool value =
                input_range_impl<T, Diagnostic>::value && compiles<Diagnostic, T, forward_iterator>;
        };

        template <class T>
        constexpr bool forward_range = forward_range_impl<T>::value;

        template <class T, bool Diagnostic = false>
        struct bidirectional_range_impl
        {
            template <typename U>
            using bidir_iter =
                RAH2_STD::enable_if_t<bidirectional_iterator_impl<iterator_t<U>, Diagnostic>::value>;

            static constexpr bool value =
                forward_range_impl<T, Diagnostic>::value && compiles<Diagnostic, T, bidir_iter>;
        };

        template <class T>
        constexpr bool bidirectional_range = bidirectional_range_impl<T>::value;

        template <class T, bool Diagnostic = false>
        struct random_access_range_impl
        {
            template <typename U>
            using has_random_access_iterator =
                RAH2_STD::enable_if_t<random_access_iterator_impl<iterator_t<U>, Diagnostic>::value>;

            static constexpr bool value = bidirectional_range_impl<T, Diagnostic>::value
                                          && compiles<Diagnostic, T, has_random_access_iterator>;
        };

        template <class T>
        constexpr bool random_access_range = random_access_range_impl<T>::value;

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

        template <typename R, bool Diagnostic = false>
        struct contiguous_range_impl
        {
            template <typename T>
            using has_data = RAH2_STD::enable_if_t<RAH2_NS::is_same_v<
                decltype(data(RAH2_STD::declval<T>())),
                RAH2_STD::add_pointer_t<range_reference_t<T>>>>;
            template <typename T>
            using contiguous_iterator =
                RAH2_STD::enable_if_t<RAH2_NS::contiguous_iterator_impl<iterator_t<T>, Diagnostic>::value>;

            static constexpr bool value = random_access_range_impl<R, Diagnostic>::value
                                          && compiles<Diagnostic, R, contiguous_iterator>
                                          && compiles<Diagnostic, R, has_data>;
        };

        template <class R>
        constexpr bool contiguous_range = contiguous_range_impl<R>::value;

        template <class T, bool Diagnostic = false>
        struct constant_range_impl
        {
            template <class U>
            static constexpr bool constant_iterator =
                input_iterator<U>
                && RAH2_NS::is_same_v<iter_const_reference_t<U>, iter_reference_t<U>>;

            template <typename U>
            using const_iter = RAH2_STD::enable_if_t<constant_iterator<iterator_t<U>>>;

            static constexpr bool value =
                input_range_impl<T, Diagnostic>::value && compiles<Diagnostic, T, const_iter>;
        };

        template <class T>
        constexpr bool constant_range = constant_range_impl<T>::value;

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
            using iter_cat = RAH2_NS::details::iterator_category<I>;

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
        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<
                !RAH2_NS::sized_sentinel_for<S, I>
                && RAH2_NS::details::weakly_equality_comparable_with<S, I>>* = nullptr>
        RAH2_NS::iter_difference_t<I> distance(I first, S last)
        {
            iter_difference_t<I> len = 0;
            for (; first != last; ++first, ++len)
            {
            }
            return len;
        }

        template <typename I, typename S, RAH2_STD::enable_if_t<RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
        auto distance(I first, S last)
        {
            return last - first;
        }
        template <typename R>
        auto distance(R&& r)
        {
            return RAH2_NS::ranges::distance(begin(r), end(r));
        }

        template <typename I, typename S, RAH2_STD::enable_if_t<RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
        constexpr intptr_t advance(I& i, iter_difference_t<I> n, S const& bound)
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
                    i = RAH2_STD::move(bound);
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
        constexpr intptr_t advance(I& i, iter_difference_t<I> n, S const& bound)
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
        constexpr intptr_t advance(I& i, iter_difference_t<I> n, S const& bound)
        {
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
        constexpr void advance(I& i, RAH2_NS::iter_difference_t<I> n)
        {
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
        constexpr void advance(I& i, RAH2_NS::iter_difference_t<I> n)
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
        constexpr void advance(I& i, RAH2_NS::iter_difference_t<I> n)
        {
            i += n;
        }

        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr,
            RAH2_STD::enable_if_t<RAH2_NS::assignable_from<I&, S>>* = nullptr>
        constexpr void advance(I& i, S bound)
        {
            i = RAH2_STD::move(bound);
        }

        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr,
            RAH2_STD::enable_if_t<not RAH2_NS::assignable_from<I&, S>>* = nullptr,
            RAH2_STD::enable_if_t<RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
        constexpr void advance(I& i, S bound)
        {
            advance(i, bound - i);
        }

        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr,
            RAH2_STD::enable_if_t<not RAH2_NS::assignable_from<I&, S>>* = nullptr,
            RAH2_STD::enable_if_t<not RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
        constexpr void advance(I& i, S bound)
        {
            while (i != bound)
                ++i;
        }

        template <typename I, RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I>>* = nullptr>
        constexpr I next(I i)
        {
            ++i;
            return i;
        }

        template <typename I, RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I>>* = nullptr>
        constexpr I next(I i, RAH2_NS::iter_difference_t<I> n)
        {
            RAH2_NS::ranges::advance(i, n);
            return i;
        }

        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr>
        constexpr I next(I i, S bound)
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
            RAH2_STD::enable_if_t<RAH2_NS::input_or_output_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr>
        constexpr I next(I i, RAH2_NS::iter_difference_t<I> n, S bound)
        {
            RAH2_NS::ranges::advance(i, n, bound);
            return i;
        }

    } // namespace ranges

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
    // ************************************** optional ********************************************
    namespace details
    {

        // Small optional impl for C++14 compilers
        template <typename T>
        struct optional
        {
            optional() = default;
            optional(optional const& other)
            {
                if (other.has_value())
                {
                    new (get_ptr()) T(other.value());
                    is_allocated_ = true;
                }
            }
            optional(optional&& other) noexcept
            {
                (*this) = RAH2_STD::move(other);
            }
            optional& operator=(optional const& other)
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
            optional& operator=(optional&& other) noexcept
            {
                if (has_value())
                {
                    if (other.has_value())
                    {
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
            optional(T const& other)
                : is_allocated_(true)
            {
                new (get_ptr()) T(other);
            }
            optional(T&& other)
                : is_allocated_(true)
            {
                new (get_ptr()) T(RAH2_STD::move(other));
            }
            template <typename... Args>
            explicit optional(RAH2_NS::in_place_t, Args&&... args)
                : is_allocated_(true)
            {
                new (get_ptr()) T(RAH2_STD::forward<Args>(args)...);
            }
            optional& operator=(T const& other)
            {
                reset();
                new (get_ptr()) T(other);
                is_allocated_ = true;
                return *this;
            }
            optional& operator=(T&& other)
            {
                reset();
                new (get_ptr()) T(RAH2_STD::move(other));
                is_allocated_ = true;
                return *this;
            }
            ~optional()
            {
                reset();
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
                return reinterpret_cast<T*>(&value_);
            }
            T const* get_ptr() const
            {
                return reinterpret_cast<T const*>(&value_);
            }
            void destruct_value()
            {
                value().~T();
            }
            RAH2_STD::aligned_storage_t<sizeof(T), RAH2_NS::alignment_of_v<T>> value_{};
            bool is_allocated_ = false;
        };
    } // namespace details
} // namespace RAH2_NS
