#pragma once

#include <type_traits>
#include <iterator>

#include "concepts.hpp"

#define RAH_STD std
#define RAH_NAMESPACE rah

#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 202000L) || __cplusplus >= 2020000L)
#define RAH_CPP20 1
#else
#define RAH_CPP20 0
#endif

namespace rah
{
    template <typename T>
    struct DeleteCheck
    {
        enum class State
        {
            VALID = 0x91AC1F0,
            DELETED = 0xCCDA459
        } state_ = State::VALID;

        DeleteCheck() = default;
        DeleteCheck(DeleteCheck const& rhs)
        {
            assert(state_ == State::VALID);
            assert(rhs.state_ == State::VALID);
        }
        DeleteCheck& operator=(DeleteCheck const& rhs)
        {
            assert(state_ == State::VALID);
            assert(rhs.state_ == State::VALID);
            return *this;
        }
        DeleteCheck(DeleteCheck&& rhs)
        {
            assert(state_ == State::VALID);
            assert(rhs.state_ == State::VALID);
        }
        DeleteCheck& operator=(DeleteCheck&& rhs)
        {
            assert(state_ == State::VALID);
            assert(rhs.state_ == State::VALID);
            return *this;
        }

        ~DeleteCheck()
        {
            assert(state_ == State::VALID);
            state_ = State::DELETED;
        }

        void check() const
        {
            assert(state_ == State::VALID);
        }
    };

#define MAKE_CONCEPT(NAME, CHECK, REQUIRE)                                                         \
    template <typename T, typename = int>                                                          \
    struct NAME##_impl                                                                             \
    {                                                                                              \
        static constexpr bool value = false;                                                       \
    };                                                                                             \
    template <typename T>                                                                          \
    struct NAME##_impl<T, decltype(std::enable_if_t<CHECK, int>{}, REQUIRE, 0)>                    \
    {                                                                                              \
        static constexpr bool value = true;                                                        \
    };                                                                                             \
    template <typename T>                                                                          \
    constexpr bool NAME = NAME##_impl<T>::value;

#define MAKE_CONCEPT_2(NAME, CHECK, REQUIRE)                                                       \
    template <typename U, typename V, bool check, typename = int>                                  \
    struct NAME##_impl                                                                             \
    {                                                                                              \
        static constexpr bool value = false;                                                       \
    };                                                                                             \
    template <typename U, typename V>                                                              \
    struct NAME##_impl<U, V, true, decltype(REQUIRE, 0)>                                           \
    {                                                                                              \
        static constexpr bool value = true;                                                        \
    };                                                                                             \
    template <typename U, typename V>                                                              \
    constexpr bool NAME = NAME##_impl<U, V, CHECK>::value;

    // **************************** standard traits ***********************************************

    using std::bidirectional_iterator_tag;
    using std::forward_iterator_tag;
    using std::input_iterator_tag;
    using std::output_iterator_tag;
    using std::random_access_iterator_tag;
    struct contiguous_iterator_tag : std::random_access_iterator_tag
    {
    };

    template <class T>
    struct remove_cvref
    {
        typedef RAH_STD::remove_cv_t<RAH_STD::remove_reference_t<T>> type;
    };
    template <class T>
    using remove_cvref_t = typename remove_cvref<T>::type;

    template <class Derived, class Base>
    constexpr bool derived_from =
        RAH_STD::is_base_of_v<Base, Derived>
        && RAH_STD::is_convertible_v<const volatile Derived*, const volatile Base*>;

    namespace details
    {
        // TODO : Replace by std::decltype
        /// Used in decltype to get an instance of a type
        template <typename T>
        T& declval();

        template <typename I, std::enable_if_t<std::is_pointer_v<I>>* = nullptr>
        auto get_iterator_category_impl() -> rah::contiguous_iterator_tag;

        template <typename I, std::enable_if_t<!std::is_pointer_v<I>>* = nullptr>
        auto get_iterator_category_impl() -> typename std::iterator_traits<I>::iterator_category;

        template <typename I>
        using iterator_category = decltype(get_iterator_category_impl<I>());
    } // namespace details

    struct view_base
    {
    };

    // **************************** range access **************************************************

    MAKE_CONCEPT(has_begin_member, true, (details::declval<T>().begin()));
    MAKE_CONCEPT(has_begin_ADL, true, (begin(details::declval<T>())));
    MAKE_CONCEPT(has_end_member, true, (details::declval<T>().end()));
    MAKE_CONCEPT(has_end_ADL, true, (end(details::declval<T>())));

    namespace details
    {
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

            template <typename R, std::enable_if_t<has_begin_member<R>>* = nullptr>
            auto operator()(R&& range) const
            {
                return range.begin();
            }

            template <typename R, std::enable_if_t<not has_begin_member<R> && has_begin_ADL<R>>* = nullptr>
            auto operator()(R&& range) const
            {
                return begin(range);
            }
        };
    } // namespace details

    inline namespace begin_ns
    {
        constexpr auto begin = details::begin_impl();
    }

    template <class T, size_t N>
    T* end(T (&array)[N]) noexcept
    {
        return array + N;
    }

    template <class T, size_t N>
    T const* end(T const (&array)[N]) noexcept
    {
        return array + N;
    }

    template <typename R, std::enable_if_t<has_end_member<R>>* = nullptr>
    auto end(R&& range)
    {
        return range.end();
    }

    template <typename R, std::enable_if_t<not has_end_member<R> and has_end_ADL<R>>* = nullptr>
    auto end(R&& range)
    {
        return end(range);
    }

    template <class T, size_t N>
    T const* cbegin(T const (&array)[N])
    {
        return array;
    }

    template <typename R>
    auto cbegin(R const& range)
    {
        return range.begin();
    }

    template <class T, size_t N>
    T const* cend(T const (&array)[N]) noexcept
    {
        return array + N;
    }

    template <typename R>
    auto cend(R const& range)
    {
        return range.end();
    }

    MAKE_CONCEPT(has_rbegin_member, true, details::declval<std::remove_reference_t<T>>().rbegin())
    MAKE_CONCEPT(has_rbegin_ADL, true, rbegin(details::declval<std::remove_reference_t<T>>()))
    MAKE_CONCEPT(has_rend_member, true, details::declval<std::remove_reference_t<T>>().rend())
    MAKE_CONCEPT(has_rend_ADL, true, rend(details::declval<std::remove_reference_t<T>>()))

    template <typename R, bool Diagnostic = false>
    struct range_impl
    {
        template <typename R2>
        static auto check(R2 r)
            -> concepts::TypeList<decltype(RAH_NAMESPACE::begin(r)), decltype(RAH_NAMESPACE::end(r))>;

        template <typename R2>
        using check_wrapper = decltype(check(std::declval<R2>()));

        static constexpr bool value = compiles<Diagnostic, R, check_wrapper>;
    };

    template <typename R>
    constexpr bool range = range_impl<R>::value;

    template <typename R, typename = std::enable_if_t<has_rbegin_member<remove_cvref_t<R>>>>
    auto rbegin(R&& range)
    {
        return range.rbegin();
    }

    template <
        typename R,
        typename = std::enable_if_t<not has_rbegin_member<remove_cvref_t<R>>>,
        typename = std::enable_if_t<has_rbegin_ADL<remove_cvref_t<R>>>>
    auto rbegin(R&& range) -> decltype(rbegin(range))
    {
        return rbegin(range);
    }

    // To mark a range as an iterator/sentinel range, event if begin/end have the same type.
    // Since begin/end can't have a different type in C++ pre-17
    //struct sentinel_range_base
    //{
    //};

    //struct maybe_sentinel_iterator
    //{
    //    bool sentinel = false;
    //};

    template <typename T>
    using iterator_t = decltype(RAH_NAMESPACE::begin(std::declval<T>()));

    template <typename T>
    using sentinel_t = decltype(RAH_NAMESPACE::end(std::declval<T&>()));

    template <class T>
    constexpr bool common_range = range<T> && RAH_STD::is_same_v<iterator_t<T>, sentinel_t<T>>;

    template <
        typename R,
        typename = std::enable_if_t<not has_rbegin_member<remove_cvref_t<R>>>,
        typename = std::enable_if_t<not has_rbegin_ADL<remove_cvref_t<R>>>,
        typename = std::enable_if_t<common_range<R>>>
    auto rbegin(R&& range)
    {
        return std::make_reverse_iterator(RAH_NAMESPACE::end(range));
    }

    template <typename R, typename = std::enable_if_t<has_rend_member<remove_cvref_t<R>>>>
    auto rend(R&& range)
    {
        return range.rend();
    }

    template <
        typename R,
        typename = std::enable_if_t<not has_rend_member<remove_cvref_t<R>>>,
        typename = std::enable_if_t<has_rend_ADL<remove_cvref_t<R>>>>
    auto rend(R&& range) -> decltype(rend(range))
    {
        return rend(range);
    }

    template <
        typename R,
        typename = std::enable_if_t<not has_rend_member<remove_cvref_t<R>>>,
        typename = std::enable_if_t<not has_rend_ADL<remove_cvref_t<R>>>,
        typename = std::enable_if_t<common_range<R>>>
    auto rend(R&& range)
    {
        return std::make_reverse_iterator(RAH_NAMESPACE::begin(range));
    }

    template <typename R>
    auto crbegin(R const& range) -> decltype(range.crbegin())
    {
        return range.crbegin();
    }

    template <typename R>
    auto crend(R const& range) -> decltype(range.crend())
    {
        return range.crend();
    }

    template <class T, size_t N>
    size_t size(T const (&)[N]) noexcept
    {
        return N;
    }

    template <typename R>
    auto size(R const& range) -> decltype(range.size())
    {
        return range.size();
    }

    template <typename R>
    auto ssize(R const& range) -> decltype(range.ssize())
    {
        return range.ssize();
    }

    template <typename R>
    auto empty(R const& range) -> decltype(range.empty())
    {
        return range.empty();
    }

    template <typename R>
    auto data(R&& range) -> decltype(range.data())
    {
        return range.data();
    }

    template <typename R>
    auto cdata(R const& range) -> decltype(range.cdata())
    {
        return range.cdata();
    }

    // *************************** iterator concepts **********************************************

    template <class I>
    constexpr bool output_iterator =
        derived_from<typename RAH_STD::iterator_traits<I>::iterator_category, RAH_STD::output_iterator_tag>;

    template <typename I, typename C = void>
    struct iter_difference;

    template <class I>
    struct incrementable_traits
    {
    };

    namespace details
    {
        template <typename I, std::enable_if_t<compiles<false, remove_cvref_t<I>, std::iterator_traits>>* = nullptr>
        typename std::iterator_traits<remove_cvref_t<I>>::difference_type get_iter_difference();

        template <
            typename I,
            std::enable_if_t<not compiles<false, remove_cvref_t<I>, std::iterator_traits>>* = nullptr>
        typename incrementable_traits<remove_cvref_t<I>>::difference_type get_iter_difference();

    } // namespace details

    template <typename I>
    using iter_difference_t = ptrdiff_t;

    template <class T>
    constexpr bool is_destructible_v = std::is_destructible<T>::value;

    template <class T, class... Args>
    constexpr bool is_constructible_v = std::is_constructible<T, Args...>::value;

    template <class T, class... Args>
    constexpr bool constructible_from = is_destructible_v<T> && is_constructible_v<T, Args...>;

    template <class From, class To>
    constexpr bool is_convertible_v = std::is_convertible<From, To>::value;

    template <class From, class To>
    struct convertible_to_impl
    {
        template <class F>
        using can_static_cast = decltype(static_cast<To>(std::declval<F>()));

        static constexpr bool value =
            is_convertible_v<From, To> && compiles<false, From, can_static_cast>;
    };

    template <class From, class To>
    constexpr bool convertible_to = convertible_to_impl<From, To>::value;

    template <class T>
    constexpr bool move_constructible =
        RAH_NAMESPACE::constructible_from<T, T> && RAH_NAMESPACE::convertible_to<T, T>;

    template <class U, typename V>
    constexpr bool same_as = std::is_same_v<U, V>;

    template <class LHS, class RHS>
    struct assignable_from_impl
    {
        template <class LHS_>
        using check_assign = std::enable_if_t<
            std::is_same_v<decltype(std::declval<LHS_>() = std::forward<RHS>(std::declval<RHS&&>())), LHS_>>;

        static constexpr bool value =
            std::is_lvalue_reference_v<LHS> && compiles<false, LHS, check_assign>;
    };

    template <class LHS, class RHS>
    constexpr bool assignable_from = assignable_from_impl<LHS, RHS>::value;

    MAKE_CONCEPT(swappable, true, std::swap(details::declval<T>(), details::declval<T>()));

    template <class T>
    constexpr bool is_object_v = std::is_object<T>::value;

    template <class T>
    constexpr bool movable = RAH_NAMESPACE::is_object_v<T> && RAH_NAMESPACE::move_constructible<T>
                             && RAH_NAMESPACE::assignable_from<T&, T> && RAH_NAMESPACE::swappable<T>;

    template <typename I>
    struct weakly_incrementable_impl
    {
        template <class T>
        using diff_is_signed_integer = std::enable_if_t<
            std::is_integral_v<iter_difference_t<T>> && std::is_signed_v<iter_difference_t<T>>>;
        template <class T>
        using incr_type = std::enable_if_t<std::is_same_v<decltype(++std::declval<T>()), T&>>;

        template <class T>
        using can_post_incr = decltype(std::declval<T&>()++);

        static constexpr bool value = movable<I> && compiles<false, I, diff_is_signed_integer>
                                      && compiles<false, I, can_post_incr>;

        struct Check
        {
            static_assert(movable<I>, "movable<I>");
            static_assert(
                compiles<false, I, diff_is_signed_integer>, "compiles<I, diff_is_signed_integer>");
            static_assert(compiles<false, I, can_post_incr>, "compiles<I, can_post_incr>");
        };
    };

    template <typename T>
    constexpr bool weakly_incrementable = weakly_incrementable_impl<T>::value;

    template <typename R>
    struct input_or_output_iterator_impl
    {
        template <class T>
        using can_ref = std::enable_if_t<not std::is_same_v<decltype(*std::declval<T>()), void>>;

        static constexpr bool value = weakly_incrementable<R> && compiles<false, R, can_ref>;

        struct Check
        {
            typename weakly_incrementable_impl<R>::Check incr_check;
            static_assert(weakly_incrementable<R>, "weakly_incrementable<R>");
            static_assert(compiles<false, R, can_ref>, "compiles<R, can_ref>");
        };
    };

    template <typename T>
    constexpr bool input_or_output_iterator = input_or_output_iterator_impl<T>::value;

    template <class T>
    using iter_value_t = typename RAH_STD::iterator_traits<T>::value_type;

    template <class T>
    using iter_reference_t = decltype(*RAH_STD::declval<T&>());

    template <typename T>
    using iter_rvalue_reference_t = decltype(std::move(*RAH_STD::declval<T&>()));

    template <class T>
    using iter_const_reference_t = decltype(*RAH_STD::declval<T const&>());

    template <typename In>
    struct indirectly_readable_impl
    {
        template <class T>
        using has_value = iter_value_t<T>;
        template <class T>
        using has_ref = iter_reference_t<T>;
        template <class T>
        using has_rval_ref = iter_rvalue_reference_t<T>;
        template <class T>
        using deref =
            std::enable_if_t<std::is_same_v<decltype(*std::declval<T>()), iter_reference_t<T>>>;
        template <class T>
        using iter_move = std::enable_if_t<
            std::is_same_v<decltype(iter_move(std::declval<T>())), iter_rvalue_reference_t<T>>>;

        // TODO re-add checks
        static constexpr bool value =
            // compiles<In, has_value> &&
            // compiles<In, has_ref> &&
            // compiles<In, has_rval_ref> &&
            compiles<false, In, deref>
            // && compiles<In, iter_move>
            ;
    };

    template <typename T>
    constexpr bool indirectly_readable = indirectly_readable_impl<remove_cvref_t<T>>::value;

    template <typename I>
    struct input_iterator_impl
    {
        static constexpr bool value =
            input_or_output_iterator<I> && indirectly_readable<I>
            && compiles<false, I, details::iterator_category>
            && derived_from<details::iterator_category<I>, std::input_iterator_tag>;

        struct Check
        {
            typename input_or_output_iterator_impl<I>::Check input_or_output_check;
            static_assert(input_or_output_iterator<I>, "input_or_output_iterator<I>");
            static_assert(indirectly_readable<I>, "indirectly_readable<I>");
            static_assert(
                compiles<false, I, details::iterator_category>,
                "compiles<I, details::iterator_category>");
            static_assert(
                derived_from<details::iterator_category<I>, std::input_iterator_tag>,
                "derived_from<details::iterator_category<I>, std::input_iterator_tag>");
        };
    };

    template <typename T>
    constexpr bool input_iterator = input_iterator_impl<T>::value;

    template <class T, class U, typename>
    struct __WeaklyEqualityComparableWith
    {
        constexpr static bool value = false;
    };

    template <class T, class U>
    struct __WeaklyEqualityComparableWith<
        T,
        U,
        decltype(std::declval<T>() == std::declval<U>(), std::declval<T>() != std::declval<U>(), std::declval<U>() == std::declval<T>(), std::declval<U>() != std::declval<T>())>
    {
        constexpr static bool value = true;
    };

#define VAL_V std::declval<V&>()
#define VAL_U std::declval<U&>()

    MAKE_CONCEPT_2(
        WeaklyEqualityComparableWith,
        true,
        VAL_V == VAL_U || VAL_V != VAL_U || VAL_U == VAL_V || VAL_U != VAL_V);

    template <class T>
    constexpr bool equality_comparable = WeaklyEqualityComparableWith<T, T>;

    template <class T>
    constexpr bool destructible = std::is_nothrow_destructible_v<T>;

    template <class T>
    constexpr bool copy_constructible =
        RAH_NAMESPACE::move_constructible<T> && RAH_NAMESPACE::constructible_from<T, T&>
        && RAH_NAMESPACE::convertible_to<T&, T> && RAH_NAMESPACE::constructible_from<T, const T&>
        && RAH_NAMESPACE::convertible_to<const T&, T>
        && RAH_NAMESPACE::constructible_from<T, const T> && RAH_NAMESPACE::convertible_to<const T, T>;

    template <class T>
    constexpr bool copyable =
        RAH_NAMESPACE::copy_constructible<T> && RAH_NAMESPACE::movable<T>
        && RAH_NAMESPACE::assignable_from<T&, T&> && RAH_NAMESPACE::assignable_from<T&, const T&>
        && RAH_NAMESPACE::assignable_from<T&, const T>;

    MAKE_CONCEPT(default_initializable, RAH_NAMESPACE::constructible_from<T>, T{});

    template <class T>
    constexpr bool semiregular =
        RAH_NAMESPACE::copyable<T> && RAH_NAMESPACE::default_initializable<T>;

    template <class S, class I>
    constexpr bool sentinel_for =
        RAH_NAMESPACE::semiregular<S> && RAH_NAMESPACE::input_or_output_iterator<I>
        && WeaklyEqualityComparableWith<I, S>;

    template <class T>
    constexpr bool regular = semiregular<T> && equality_comparable<T>;

    template <typename I>
    struct incrementable_impl
    {
        template <typename U>
        using check_incr = std::enable_if_t<std::is_same_v<decltype(std::declval<I&>()++), I>>;

        static constexpr bool value =
            regular<I> && weakly_incrementable<I> && compiles<false, I, check_incr>;
    };
    template <typename I>
    constexpr bool incrementable = incrementable_impl<I>::value;

    template <class S, class I>
    static constexpr bool disable_sized_sentinel_for = false;

    MAKE_CONCEPT_2(
        sized_sentinel_for,
        (RAH_NAMESPACE::sentinel_for<U, V>
         && !RAH_NAMESPACE::disable_sized_sentinel_for<std::remove_cv_t<U>, std::remove_cv_t<V>>),
        (std::declval<U>() - std::declval<V>(), std::declval<V>() - std::declval<U>()));

    namespace details
    {
        template <typename T, typename U, bool Diagnostic = false>
        struct partially_ordered_with_impl
        {
            using NoRefT = std::remove_reference_t<T>;
            using NoRefU = std::remove_reference_t<U>;
            template <typename X, typename Y>
            using t_lesser_u = decltype((std::declval<X>() < std::declval<Y>()) == true);
            template <typename X, typename Y>
            using t_greater_u = decltype((std::declval<X>() > std::declval<Y>()) == true);
            template <typename X, typename Y>
            using t_lesserequal_u = decltype((std::declval<X>() <= std::declval<Y>()) == true);
            template <typename X, typename Y>
            using t_greaterequal_u = decltype((std::declval<X>() >= std::declval<Y>()) == true);

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
            is_true_v<Diagnostic, rah::equality_comparable<T>>
            && details::partially_ordered_with_impl<T, T, Diagnostic>::value;
    };

    template <class T>
    constexpr bool totally_ordered = totally_ordered_impl<T>::value;

    template <typename I, bool Diagnostic = false>
    struct forward_iterator_impl
    {
        static constexpr bool value =
            input_iterator<I>
            && derived_from<details::iterator_category<I>, std::forward_iterator_tag>
            && incrementable<I> && sentinel_for<I, I>;
    };

    template <typename I>
    constexpr bool forward_iterator = forward_iterator_impl<I>::value;

    template <typename I, bool Diagnostic = false>
    struct bidirectional_iterator_impl
    {
        template <typename U>
        static auto check(U i) -> concepts::TypeList<
            decltype(is_true<Diagnostic, forward_iterator_impl<U, Diagnostic>::value>()),
            decltype(is_true<
                     Diagnostic,
                     derived_from<details::iterator_category<U>, std::bidirectional_iterator_tag>>()),
            decltype(is_true<Diagnostic, std::is_same_v<decltype(--i), U&>>()),
            decltype(is_true<Diagnostic, std::is_same_v<decltype(i--), U>>())>;

        template <typename U = I>
        using check_wrapper = decltype(check(std::declval<U>()));

        static constexpr bool value = compiles<Diagnostic, I, check_wrapper>;
    };
    template <typename I>
    constexpr bool bidirectional_iterator = bidirectional_iterator_impl<remove_cvref_t<I>>::value;

    template <typename I, bool Diagnostic = false>
    struct random_access_iterator_impl
    {
        template <typename U>
        using addEqual = std::enable_if_t<
            std::is_same_v<decltype(std::declval<U&>() += rah::iter_difference_t<U>()), U&>>;

        template <typename U>
        using add =
            std::enable_if_t<std::is_same_v<decltype(std::declval<U>() + rah::iter_difference_t<U>()), U>>;

        template <typename U>
        using add2 =
            std::enable_if_t<std::is_same_v<decltype(rah::iter_difference_t<U>() + std::declval<U>()), U>>;

        template <typename U>
        using subEqual = std::enable_if_t<
            std::is_same_v<decltype(std::declval<U&>() -= rah::iter_difference_t<U>()), U&>>;

        template <typename U>
        using sub =
            std::enable_if_t<std::is_same_v<decltype(std::declval<U>() - rah::iter_difference_t<U>()), U>>;

        template <typename U>
        using arr = std::enable_if_t<
            std::is_same_v<decltype(std::declval<U>()[rah::iter_difference_t<U>()]), rah::iter_reference_t<U>>>;

        static constexpr bool value =
            rah::bidirectional_iterator_impl<I, Diagnostic>::value
            && is_true_v<Diagnostic, rah::derived_from<details::iterator_category<I>, std::random_access_iterator_tag>>
            && rah::totally_ordered_impl<I, Diagnostic>::value
            && is_true_v<Diagnostic, rah::sized_sentinel_for<I, I>>
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
            rah::random_access_iterator_impl<I, Diagnostic>::value
            && is_true_v<Diagnostic, rah::derived_from<details::iterator_category<I>, rah::contiguous_iterator_tag>>
            && is_true_v<Diagnostic, std::is_lvalue_reference_v<rah::iter_reference_t<I>>>
            && is_true_v<
                Diagnostic,
                rah::same_as<rah::iter_value_t<I>, rah::remove_cvref_t<rah::iter_reference_t<I>>>>;
    };
    template <typename I>
    static constexpr bool contiguous_iterator = contiguous_iterator_impl<I>::value;
    template <class T>
    constexpr bool constant_iterator =
        input_iterator<T> && RAH_STD::is_same_v<iter_const_reference_t<T>, iter_reference_t<T>>;

    template <class T>
    constexpr auto iter_move(T&& t) -> decltype(RAH_STD::move(*t))
    {
        return RAH_STD::move(*t);
    }

    // **************************** range traits **************************************************

    RAH_STD::input_iterator_tag
        get_common_iterator_tag(RAH_STD::input_iterator_tag, RAH_STD::input_iterator_tag);
    RAH_STD::output_iterator_tag
        get_common_iterator_tag(RAH_STD::output_iterator_tag, RAH_STD::output_iterator_tag);
    RAH_STD::forward_iterator_tag
        get_common_iterator_tag(RAH_STD::forward_iterator_tag, RAH_STD::forward_iterator_tag);
    RAH_STD::bidirectional_iterator_tag get_common_iterator_tag(
        RAH_STD::bidirectional_iterator_tag, RAH_STD::bidirectional_iterator_tag);
    RAH_STD::random_access_iterator_tag get_common_iterator_tag(
        RAH_STD::random_access_iterator_tag, RAH_STD::random_access_iterator_tag);

    template <typename A, typename B>
    using common_iterator_tag =
        decltype(get_common_iterator_tag(std::declval<A>(), std::declval<B>()));

    //template <typename T>
    //using iterator_t = decltype(RAH_NAMESPACE::begin(details::declval<T>()));

    template <typename T>
    using const_iterator_t = decltype(RAH_NAMESPACE::cbegin(details::declval<T>()));

    template <typename T>
    using const_sentinel_t = decltype(RAH_NAMESPACE::cend(details::declval<T>()));

    template <typename R>
    using range_size_t = decltype(RAH_NAMESPACE::size(RAH_STD::declval<R&>()));

    template <typename R>
    using range_difference_t = RAH_NAMESPACE::iter_difference_t<RAH_NAMESPACE::iterator_t<R>>;

    template <typename R>
    using range_value_t = RAH_NAMESPACE::iter_value_t<RAH_NAMESPACE::iterator_t<R>>;

    template <typename R>
    using range_reference_t = RAH_NAMESPACE::iter_reference_t<RAH_NAMESPACE::iterator_t<R>>;

    template <typename R>
    using range_const_reference_t =
        RAH_NAMESPACE::iter_const_reference_t<RAH_NAMESPACE::iterator_t<R>>;

    template <typename R>
    using range_rvalue_reference_t =
        RAH_NAMESPACE::iter_rvalue_reference_t<RAH_NAMESPACE::iterator_t<R>>;

    template <typename R>
    using range_iter_categ_t =
        typename RAH_STD::iterator_traits<RAH_NAMESPACE::iterator_t<R>>::iterator_category;

    // ******************************** ranges concepts *******************************************

    template <class R>
    constexpr bool enable_borrowed_range = false;

    template <class R>
    constexpr bool borrowed_range =
        range<R> && (RAH_STD::is_lvalue_reference_v<R> || enable_borrowed_range<remove_cvref_t<R>>);

    template <class>
    constexpr bool disable_sized_range = false;

    template <typename R, typename = int>
    struct has_ranges_size
    {
        static constexpr bool value = false;
    };

    template <typename R>
    struct has_ranges_size<R, decltype(RAH_NAMESPACE::size(details::declval<R>()), 0)>
    {
        static constexpr bool value = true;
    };

    template <class T>
    constexpr bool sized_range = range<T> && has_ranges_size<T>::value && !(disable_sized_range<T>);

    template <typename V>
    struct view_interface;

    template <typename R>
    constexpr bool enable_view = derived_from<R, view_base> || derived_from<R, view_interface<R>>;

    template <typename T>
    constexpr bool view = range<T> && enable_view<T>;

    template <class T>
    constexpr bool output_range = range<T> && output_iterator<RAH_NAMESPACE::iterator_t<T>>;

    template <class T>
    constexpr bool input_range = range<T> && input_iterator<RAH_NAMESPACE::iterator_t<T>>;

    template <class T>
    constexpr bool forward_range = range<T> && forward_iterator<RAH_NAMESPACE::iterator_t<T>>;

    template <class T, bool Diagnostic = false>
    struct bidirectional_range_impl
    {
        template <typename U = T>
        using check = concepts::TypeList<
            decltype(is_true<Diagnostic, range_impl<U, Diagnostic>::value>()),
            decltype(is_true<
                     Diagnostic,
                     bidirectional_iterator_impl<RAH_NAMESPACE::iterator_t<U>, Diagnostic>::value>())>;

        static constexpr bool value = compiles<Diagnostic, T, check>;
    };

    template <class T>
    constexpr bool bidirectional_range = bidirectional_range_impl<T>::value;

    template <class T, bool Diagnostic = false>
    struct random_access_range_impl
    {
        template <typename U>
        using has_random_access_iterator =
            std::enable_if_t<random_access_iterator_impl<RAH_NAMESPACE::iterator_t<U>, Diagnostic>::value>;

        static constexpr bool value =
            range_impl<T, Diagnostic>::value && compiles<Diagnostic, T, has_random_access_iterator>;
    };

    template <class T>
    constexpr bool random_access_range = random_access_range_impl<T>::value;

    template <typename R, typename = int>
    struct has_ranges_data
    {
        static constexpr bool value = false;
    };

    template <typename R>
    struct has_ranges_data<R, decltype(RAH_NAMESPACE::data(details::declval<R>()), 0)>
    {
        static constexpr bool value = true;
    };

    template <typename R, bool Diagnostic = false>
    struct contiguous_range_impl
    {
        template <typename T>
        static auto requ(T& t) -> concepts::TypeList<std::enable_if_t<
            std::is_same_v<decltype(rah::data(t)), std::add_pointer_t<rah::range_reference_t<T>>>>>;

        template <typename U>
        using check = concepts::TypeList<
            std::enable_if_t<random_access_range_impl<U, Diagnostic>::value>,
            std::enable_if_t<rah::contiguous_iterator_impl<rah::iterator_t<U>, Diagnostic>::value>,
            decltype(requ(std::declval<U&>()))>;

        static constexpr bool value = compiles<Diagnostic, R, check>;
    };

    template <class R>
    constexpr bool contiguous_range = contiguous_range_impl<R>::value;

    template <class T>
    constexpr bool constant_range =
        input_range<T> && constant_iterator<RAH_NAMESPACE::iterator_t<T>>;

    // ****************************** utility functions *******************************************

    template <typename I, typename S, typename = std::enable_if_t<RAH_NAMESPACE::sized_sentinel_for<S, I>>>
    constexpr intptr_t advance(I& i, intptr_t n, S const& bound)
    {
        // std::abs is not constexpr until C++23
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
                i = std::move(bound);
                return n - distToBound;
            }
        }
        RAH_STD::advance(i, n);
        return 0;
    }
    template <
        typename I,
        typename S,
        typename = std::enable_if_t<not RAH_NAMESPACE::sized_sentinel_for<S, I>>,
        typename = std::enable_if_t<RAH_NAMESPACE::bidirectional_iterator<I>>>
    constexpr intptr_t advance(I& i, intptr_t n, S const& bound)
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
        typename = std::enable_if_t<not RAH_NAMESPACE::sized_sentinel_for<S, I>>,
        typename = std::enable_if_t<not RAH_NAMESPACE::bidirectional_iterator<I>>,
        int = 0>
    constexpr intptr_t advance(I& i, intptr_t n, S const& bound)
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
        std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I>>* = nullptr,
        std::enable_if_t<not RAH_NAMESPACE::bidirectional_iterator<I>>* = nullptr>
    constexpr void advance(I& i, RAH_NAMESPACE::iter_difference_t<I> n)
    {
        while (n > 0)
        {
            --n;
            ++i;
        }
    }

    template <
        typename I,
        std::enable_if_t<RAH_NAMESPACE::bidirectional_iterator<I>>* = nullptr,
        std::enable_if_t<not RAH_NAMESPACE::random_access_iterator<I>>* = nullptr>
    constexpr void advance(I& i, RAH_NAMESPACE::iter_difference_t<I> n)
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

    template <typename I, std::enable_if_t<RAH_NAMESPACE::random_access_iterator<I>>* = nullptr>
    constexpr void advance(I& i, RAH_NAMESPACE::iter_difference_t<I> n)
    {
        i += n;
    }

    template <
        typename I,
        typename S,
        std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>>* = nullptr,
        std::enable_if_t<RAH_NAMESPACE::assignable_from<I&, S>>* = nullptr,
        std::enable_if_t<not RAH_NAMESPACE::sized_sentinel_for<S, I>>* = nullptr>
    constexpr void advance(I& i, S bound)
    {
        i = std::move(bound);
    }

    template <
        typename I,
        typename S,
        std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>>* = nullptr,
        std::enable_if_t<not RAH_NAMESPACE::assignable_from<I&, S>>* = nullptr,
        std::enable_if_t<RAH_NAMESPACE::sized_sentinel_for<S, I>>* = nullptr>
    constexpr void advance(I& i, S bound)
    {
        advance(i, bound - i);
    }

    template <
        typename I,
        typename S,
        std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>>* = nullptr,
        std::enable_if_t<not RAH_NAMESPACE::assignable_from<I&, S>>* = nullptr,
        std::enable_if_t<not RAH_NAMESPACE::sized_sentinel_for<S, I>>* = nullptr>
    constexpr void advance(I& i, S bound)
    {
        while (i != bound)
            ++i;
    }

    /// Apply the '<' operator on two values of any type
    struct is_lesser
    {
        template <typename A, typename B>
        bool operator()(A&& a, B&& b)
        {
            return a < b;
        }
    };

    // ******************************* views ******************************************************

#define RAH_SELF (*static_cast<T* const>(this))
#define RAH_SELF_CONST (*static_cast<T const* const>(this))

    template <typename T>
    struct view_interface : view_base
    {
        DeleteCheck<view_interface<T>> deleteCheck;

        auto empty()
        {
            return RAH_SELF.begin() == RAH_SELF.end();
        }

        //template <typename = std::enable_if_t<RAH_NAMESPACE::forward_range<T>>>
        //operator bool() const
        //{
        //    return RAH_SELF_CONST.begin() != RAH_SELF_CONST.end();
        //}
        template <
            typename D = T,
            std::enable_if_t<
                RAH_NAMESPACE::forward_range<D>
                && RAH_NAMESPACE::
                    sized_sentinel_for<RAH_NAMESPACE::sentinel_t<D>, RAH_NAMESPACE::iterator_t<D>>>* = nullptr>
        auto size()
        {
            return RAH_SELF.begin() - RAH_SELF.end();
        }

        template <
            typename D = T,
            std::enable_if_t<not(
                RAH_NAMESPACE::forward_range<D>
                && RAH_NAMESPACE::
                    sized_sentinel_for<RAH_NAMESPACE::sentinel_t<D>, RAH_NAMESPACE::iterator_t<D>>)>* = nullptr,
            std::enable_if_t<
                RAH_NAMESPACE::forward_range<const D>
                && RAH_NAMESPACE::sized_sentinel_for<
                    RAH_NAMESPACE::sentinel_t<const D>,
                    RAH_NAMESPACE::iterator_t<const D>>>* = nullptr>
        auto size() const
        {
            return RAH_SELF_CONST.begin() - RAH_SELF_CONST.end();
        }

        auto front() // -> decltype(*(details::template declval<T const>().begin()))
        {
            return *(RAH_SELF.begin());
        }

        auto back() // -> decltype(*(RAH_SELF_CONST.end()))
        {
            auto last = RAH_SELF.end();
            --last;
            return *last;
        }

        auto operator[](size_t index) // -> decltype(*(RAH_SELF_CONST.begin()))
        {
            return *(RAH_SELF.begin() + index);
        }
    };

    template <typename I, typename S = I>
    class subrange : public view_interface<subrange<I, S>>
    {
        I iterator_;
        S sentinel_;

    public:
        subrange() = default;
        subrange(I a, S b)
            : iterator_(RAH_STD::move(a))
            , sentinel_(RAH_STD::move(b))
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
    };

    template <typename I, typename S>
    auto make_subrange(I b, S e)
    {
        return subrange<I, S>{b, e};
    }

    struct default_sentinel
    {
    };

    struct unreachable_sentinel
    {
    };

    template <typename I, std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I>>* = nullptr>
    constexpr I next(I i)
    {
        ++i;
        return i;
    }

    template <typename I, std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I>>* = nullptr>
    constexpr I next(I i, RAH_NAMESPACE::iter_difference_t<I> n)
    {
        RAH_NAMESPACE::advance(i, n);
        return i;
    }

    template <
        typename I,
        typename S,
        std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I>
                         //&& RAH_NAMESPACE::sentinel_for<S, I>
                         >* = nullptr>
    constexpr I next(I i, S bound)
    {
        static_assert(RAH_NAMESPACE::semiregular<S>);
        static_assert(RAH_NAMESPACE::input_or_output_iterator<I>);
        static_assert(WeaklyEqualityComparableWith<I, S>);
        //static_assert(RAH_NAMESPACE::sentinel_for<S, I>);
        RAH_NAMESPACE::advance(i, bound);
        return i;
    }

    template <
        typename I,
        typename S,
        std::enable_if_t<RAH_NAMESPACE::input_or_output_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>>* = nullptr>
    constexpr I next(I i, RAH_NAMESPACE::iter_difference_t<I> n, S bound)
    {
        RAH_NAMESPACE::advance(i, n, bound);
        return i;
    }

} // namespace rah
