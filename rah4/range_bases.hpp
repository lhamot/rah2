#pragma once

#include <type_traits>
#include <iterator>

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
        /// Used in decltype to get an instance of a type
        template <typename T>
        T& declval();
    } // namespace details

    struct view_base
    {
    };

    // **************************** range access **************************************************

    template <class T, size_t N>
    T* begin(T (&array)[N])
    {
        return array;
    }

    template <class T, size_t N>
    T const* begin(T const (&array)[N])
    {
        return array;
    }

    MAKE_CONCEPT(has_begin_member, true, (details::declval<T>().begin()));
    MAKE_CONCEPT(has_begin_ADL, true, (begin(details::declval<T>())));
    MAKE_CONCEPT(has_end_member, true, (details::declval<T>().end()));
    MAKE_CONCEPT(has_end_ADL, true, (end(details::declval<T>())));

    template <typename R, std::enable_if_t<has_begin_member<R>>* = nullptr>
    auto begin(R&& range)
    {
        return range.begin();
    }

    template <typename R, std::enable_if_t<not has_begin_member<R> && has_begin_ADL<R>>* = nullptr>
    auto begin(R&& range)
    {
        return begin(range);
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
    MAKE_CONCEPT(
        range,
        true,
        (RAH_NAMESPACE::begin(std::declval<std::remove_reference_t<T&>>()),
         RAH_NAMESPACE::end(std::declval<std::remove_reference_t<T&>>())))

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
    using iterator_t = decltype(RAH_NAMESPACE::begin(std::declval<T&>()));

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

    template <class I>
    constexpr bool input_iterator =
        derived_from<typename RAH_STD::iterator_traits<I>::iterator_category, RAH_STD::input_iterator_tag>;

    template <class I>
    constexpr bool input_or_output_iterator = input_iterator<I> or output_iterator<I>;

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

    template <class T, class... Args>
    constexpr bool constructible_from =
        RAH_NAMESPACE::destructible<T> && std::is_constructible_v<T, Args...>;

    MAKE_CONCEPT_2(convertible_to, (std::is_convertible_v<U, V>), (static_cast<V>(std::declval<U>())));

    template <class T>
    constexpr bool move_constructible =
        RAH_NAMESPACE::constructible_from<T, T> && RAH_NAMESPACE::convertible_to<T, T>;

    template <class T>
    constexpr bool copy_constructible =
        RAH_NAMESPACE::move_constructible<T> && RAH_NAMESPACE::constructible_from<T, T&>
        && RAH_NAMESPACE::convertible_to<T&, T> && RAH_NAMESPACE::constructible_from<T, const T&>
        && RAH_NAMESPACE::convertible_to<const T&, T>
        && RAH_NAMESPACE::constructible_from<T, const T> && RAH_NAMESPACE::convertible_to<const T, T>;

    MAKE_CONCEPT_2(
        assignable_from, (std::is_lvalue_reference_v<U>), (details::declval<U>() = std::declval<V>()));

    MAKE_CONCEPT(swappable, true, std::swap(details::declval<T>(), details::declval<T>()));

    template <class T>
    constexpr bool movable = std::is_object_v<T> && RAH_NAMESPACE::move_constructible<T>
                             && RAH_NAMESPACE::assignable_from<T&, T> && RAH_NAMESPACE::swappable<T>;

    template <class U, typename V>
    constexpr bool same_as = std::is_same_v<U, V>;

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

    MAKE_CONCEPT(
        forward_iterator,
        (derived_from<typename RAH_STD::iterator_traits<T>::iterator_category, RAH_STD::forward_iterator_tag>),
        0);

    template <class I>
    constexpr bool bidirectional_iterator =
        derived_from<typename RAH_STD::iterator_traits<I>::iterator_category, RAH_STD::bidirectional_iterator_tag>;

    template <class I>
    constexpr bool random_access_iterator =
        derived_from<typename RAH_STD::iterator_traits<I>::iterator_category, RAH_STD::random_access_iterator_tag>;

    template <class T>
    using iter_value_t = typename RAH_STD::iterator_traits<T>::value_type;

    template <class T>
    using iter_reference_t = decltype(*RAH_STD::declval<T&>());

    template <class T>
    using iter_const_reference_t = decltype(*RAH_STD::declval<T const&>());

    template <class T>
    constexpr bool constant_iterator =
        input_iterator<T> && RAH_STD::is_same_v<iter_const_reference_t<T>, iter_reference_t<T>>;

    template <class T>
    using iter_difference_t = decltype(RAH_STD::declval<T>() - RAH_STD::declval<T>());

    template <class T>
    constexpr auto iter_move(T&& t)
    {
        return RAH_STD::move(*t);
    }

    template <typename T>
    using iter_rvalue_reference_t = decltype(RAH_NAMESPACE::iter_move(RAH_STD::declval<T&>()));

    template <class S, class I>
    static constexpr bool disable_sized_sentinel_for = false;

    MAKE_CONCEPT_2(
        sized_sentinel_for,
        (RAH_NAMESPACE::sentinel_for<U, V>
         && !RAH_NAMESPACE::disable_sized_sentinel_for<std::remove_cv_t<U>, std::remove_cv_t<V>>),
        (std::declval<U>() - std::declval<V>(), std::declval<V>() - std::declval<U>()));

    // **************************** range traits **************************************************

    RAH_STD::output_iterator_tag
        get_common_iterator_tag(RAH_STD::output_iterator_tag, RAH_STD::output_iterator_tag);
    RAH_STD::forward_iterator_tag
        get_common_iterator_tag(RAH_STD::forward_iterator_tag, RAH_STD::forward_iterator_tag);
    RAH_STD::bidirectional_iterator_tag get_common_iterator_tag(
        RAH_STD::bidirectional_iterator_tag, RAH_STD::bidirectional_iterator_tag);
    RAH_STD::random_access_iterator_tag get_common_iterator_tag(
        RAH_STD::random_access_iterator_tag, RAH_STD::random_access_iterator_tag);

    template <typename A, typename B>
    using common_iterator_tag = decltype(get_common_iterator_tag(A{}, B{}));

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
    struct has_ranges_size<R, decltype(RAH_NAMESPACE::size(details::declval<R>()))>
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

    template <class T>
    constexpr bool bidirectional_range =
        range<T> && bidirectional_iterator<RAH_NAMESPACE::iterator_t<T>>;

    template <class T>
    constexpr bool random_access_range =
        range<T> && random_access_iterator<RAH_NAMESPACE::iterator_t<T>>;

    template <typename R, typename = int>
    struct has_ranges_data
    {
        static constexpr bool value = false;
    };

    template <typename R>
    struct has_ranges_data<R, decltype(RAH_NAMESPACE::data(details::declval<R>()))>
    {
        static constexpr bool value = true;
    };

    template <class R>
    constexpr bool contiguous_range = random_access_range<R> && has_ranges_data<R>::value;

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
    struct view_interface
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
    struct subrange : view_interface<subrange<I, S>>
    {
        I iterator;
        S sentinel;

        subrange() = default;
        subrange(I a, S b)
            : iterator(RAH_STD::move(a))
            , sentinel(RAH_STD::move(b))
        {
        }

        I begin() const
        {
            return iterator;
        }
        S end() const
        {
            return sentinel;
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
