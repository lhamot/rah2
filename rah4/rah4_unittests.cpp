//
// Copyright (c) 2019 Loïc HAMOT
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#include "rah4.hpp"

#include <iostream>
#include <vector>
#include <map>
#include <list>
#include <ciso646>
#include <sstream>
#include <random>
#include <atomic>
#include <array>

#include <iomanip>

#include "test_helpers.hpp"

template <typename A, typename B, typename C, typename D>
bool operator==(std::tuple<A, B> a, std::pair<D, C> b)
{
    return std::get<0>(a) == std::get<0>(b) && std::get<1>(a) == std::get<1>(b);
}

template <typename A, typename B, typename C, typename D>
bool operator==(std::pair<A, B> a, std::tuple<D, C> b)
{
    return std::get<0>(a) == std::get<0>(b) && std::get<1>(a) == std::get<1>(b);
}

auto PairEqual = [](auto&& ab)
{
    static_assert(
        RAH_NAMESPACE::WeaklyEqualityComparableWith<decltype(std::get<0>(ab)), decltype(std::get<1>(ab))>,
        "second not assignable to first");
    return std::get<0>(ab) == std::get<1>(ab);
};

#undef assert
#define assert(CONDITION)                                                                          \
    {                                                                                              \
        std::cout << __LINE__ << " assert : " << #CONDITION << std::endl;                          \
        if (CONDITION)                                                                             \
            std::cout << "OK" << std::endl;                                                        \
        else                                                                                       \
        {                                                                                          \
            std::cout << "NOT OK" << std::endl;                                                    \
            abort();                                                                               \
        }                                                                                          \
    }

template <typename R, typename I>
void equalRange(R&& RANGE, I&& IL, char const* rangeName, char const* ILName)
{
    static_assert(
        RAH_NAMESPACE::WeaklyEqualityComparableWith<
            rah::range_reference_t<decltype(RANGE)>,
            rah::range_reference_t<decltype(IL)>>,
        "Can't compare");
    std::cout << "assert : " << rangeName << " == " << ILName << std::endl;
    if (rah::views::zip(std::forward<R>(RANGE), std::forward<I>(IL)) | rah::all_of(PairEqual))
        std::cout << "OK" << std::endl;
    else
    {
        std::cout << "NOT OK" << std::endl;
        abort();
    }
}

#define EQUAL_RANGE(RANGE, IL) equalRange(RANGE, IL, #RANGE, #IL)

template <typename T>
using il = std::initializer_list<T>;

template <typename... Args>
std::ostream& operator<<(std::ostream& os, std::tuple<Args...> tup)
{
    auto print_elt = [](auto&& elt)
    {
        (std::cout << std::forward<decltype(elt)>(elt)) << " ";
    };

    ::rah::views::details::for_each(tup, print_elt);
    return os;
}

namespace test
{
    template <class T>
    constexpr bool is_reference_v = std::is_reference<T>::value;
    template <class T>
    constexpr bool is_rvalue_reference_v = std::is_rvalue_reference<T>::value;
} // namespace test

template <typename T>
struct WhatIsIt;

/// [make_pipeable create]
auto test_count(int i)
{
    return rah::make_pipeable([=](auto&& range) { return std::count(begin(range), end(range), i); });
}
/// [make_pipeable create]

template <typename R, typename = std::enable_if_t<rah::range<R>>>
void toto(R&&)
{
}

template <typename V>
auto toto(std::initializer_list<V> il)
{
    return toto(rah::make_subrange(begin(il), end(il)));
}

bool is_odd(int val)
{
    return val % 2 == 0;
}

template <typename T>
struct WhatIs;

// Test creation of a custom iterator
struct CustomGenerator
    : rah::iterator_facade<CustomGenerator, rah::default_sentinel, int, RAH_STD::forward_iterator_tag>
{
    int y = 1;

    CustomGenerator& operator++()
    {
        y *= 2;
        return *this;
    }
    auto operator*() const
    {
        return y;
    }
    bool operator==(CustomGenerator) const
    {
        return y > 10;
    }
};

auto customGenerate()
{
    return rah::subrange<CustomGenerator, CustomGenerator>{};
}

struct Add
{
    auto operator()(int i) const
    {
        return i;
    }
    template <typename... Args>
    auto operator()(int i, Args... ints) const
    {
        return i + (*this)(ints...);
    }
};

#define CHECK_EQUAL(A, B) assert(A == B)

auto inputSentView = make_test_view<Sentinel, std::input_iterator_tag, false>();
auto fwdSentView = make_test_view<Sentinel, std::forward_iterator_tag, false>();
auto fwdCommonView = make_test_view<Common, std::forward_iterator_tag, false>();
auto bidirSentView = make_test_view<Sentinel, std::bidirectional_iterator_tag, false>();
auto bidirCommonView = make_test_view<Common, std::bidirectional_iterator_tag, false>();
auto rdmSentView = make_test_view<Sentinel, std::random_access_iterator_tag, true>();
auto rdmCommonView = make_test_view<Common, std::random_access_iterator_tag, true>();
auto contiSentView = make_test_view<Sentinel, rah::contiguous_iterator_tag, true>();
auto contiCommonView = make_test_view<Common, rah::contiguous_iterator_tag, true>();

void test_counted_iterator()
{
    {
        /// [counted_iterator]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::counted(in.begin(), 5);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        /// [counted_iterator]
    }

    {
        auto iter = rah::make_counted_iterator(begin(inputSentView), 10);
        STATIC_ASSERT(rah::input_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah::forward_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(fwdSentView), 10);
        STATIC_ASSERT(rah::forward_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah::bidirectional_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(fwdCommonView), 10);
        STATIC_ASSERT(rah::forward_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah::bidirectional_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(bidirSentView), 10);
        STATIC_ASSERT((rah::bidirectional_iterator_impl<decltype(iter), true>::value));
        STATIC_ASSERT(not rah::random_access_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(bidirCommonView), 10);
        constexpr auto fsdkjfgqs = rah::bidirectional_iterator_impl<decltype(iter), true>::value;
        STATIC_ASSERT(fsdkjfgqs);
        STATIC_ASSERT(rah::bidirectional_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah::random_access_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(rdmSentView), 10);
        // STATIC_ASSERT(rah::totally_ordered<decltype(iter)>);
        STATIC_ASSERT((rah::random_access_iterator_impl<decltype(iter), true>::value));
        STATIC_ASSERT(rah::random_access_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah::contiguous_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(rdmCommonView), 10);
        STATIC_ASSERT(rah::random_access_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah::contiguous_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(contiSentView), 10);
        STATIC_ASSERT((rah::contiguous_iterator_impl<decltype(iter), true>::value));
        STATIC_ASSERT(rah::contiguous_iterator<decltype(iter)>);
    }
    {
        auto iter = rah::make_counted_iterator(begin(contiCommonView), 10);
        STATIC_ASSERT(rah::contiguous_iterator<decltype(iter)>);
    }
}

void test_empty_view()
{
    /// [empty]
    std::vector<int> result;
    for (int i : rah::views::empty<int>())
        result.push_back(i);
    assert(result == std::vector<int>());
    /// [empty]
    STATIC_ASSERT((rah::contiguous_range_impl<rah::views::empty_view<int>, true>::value));
}

void test_single_view()
{
    /// [single]
    std::vector<int> result;
    for (int i : rah::views::single(20))
        result.push_back(i);
    assert(result == std::vector<int>({20}));
    /// [single]
    STATIC_ASSERT(rah::contiguous_range<rah::views::single_view<int>>);
}

void test_iota_view()
{
    {
        /// [iota]
        std::vector<int> result;
        for (int i : rah::views::iota(10, 15))
            result.push_back(i);
        assert(result == std::vector<int>({10, 11, 12, 13, 14}));
        /// [iota]
        STATIC_ASSERT(rah::random_access_range<decltype(rah::views::iota(10, 15))>);
    }

    {
        std::vector<int> result;
        for (int i : rah::views::iota(10) | rah::views::slice(2, 5))
            result.push_back(i);
        assert(result == std::vector<int>({12, 13, 14}));
    }

    {
        std::vector<size_t> result;
        for (size_t i : rah::views::iota() | rah::views::slice(2, 5))
            result.push_back(i);
        assert(result == std::vector<size_t>({2, 3, 4}));
    }
}

void test_istream_view()
{
    /// [views::istream]
    std::stringstream ss("a b c d e f g h i j k l");
    std::vector<std::string> out;
    for (auto&& str : rah::views::istream<std::string>(ss) | rah::views::common())
    {
        out.push_back(str);
    }
    assert(
        out == std::vector<std::string>({"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"}));
    /// [views::istream]
    STATIC_ASSERT((rah::input_range<decltype(rah::views::istream<std::string>(ss))>));
}

void test_repeat_view()
{
    /// [repeat]
    std::vector<int> out;
    auto range = rah::views::repeat(42);
    std::copy_n(begin(range), 5, std::back_inserter(out));
    assert(out == std::vector<int>({42, 42, 42, 42, 42}));
    /// [repeat]
    STATIC_ASSERT((rah::random_access_range_impl<decltype(range), true>::value));
}

void test_owning_view()
{
    /// [views::owning_view]
    std::vector<int> out;
    auto owning = rah::views::owning(std::vector<int>{0, 1, 2, 2, 3});
    for (auto&& val : owning)
    {
        out.push_back(val);
    }
    assert(out == (std::vector<int>{0, 1, 2, 2, 3}));
    /// [views::owning_view]
    STATIC_ASSERT((rah::random_access_range_impl<decltype(owning), true>::value));
}

void test_all_view()
{
    // Test all
    // A views can't embbed a container
    // EQUAL_RANGE((il<int>{ 0, 1, 2, 3 } | rah::views::all()), (il<int>{ 0, 1, 2, 3 }));
    int intTab[] = {0, 1, 2, 3};
    EQUAL_RANGE((intTab | rah::views::all()), (il<int>{0, 1, 2, 3}));

    /// [views::all]
    std::vector<int> out;
    auto all = rah::views::all(std::vector<int>{0, 1, 2, 2, 3});
    for (auto&& val : all)
    {
        out.push_back(val);
    }
    assert(out == (std::vector<int>{0, 1, 2, 2, 3}));
    /// [views::all]
    STATIC_ASSERT((rah::random_access_range_impl<decltype(all), true>::value));
}

struct make_filter_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make() const
        {
            return rah::views::filter(
                make_test_view<CS, Tag, Sized>(), [](auto a) { return a % 2 == 0; });
        }
        static constexpr bool is_sized = false;
        static constexpr bool is_common = rah::common_range<test_view<CS, Tag, Sized>>;
        static constexpr bool do_test = true;
    };
};
void test_filter_view()
{
    /// [filter]
    std::vector<int> vec_01234{0, 1, 2, 3, 4, 5};
    std::vector<int> result;
    for (int i : rah::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
        result.push_back(i);
    assert(result == std::vector<int>({0, 2, 4}));
    /// [filter]

    check_all_cat<rah::bidirectional_iterator_tag, std::input_iterator_tag, make_filter_view::Trait>();
}

struct make_transform_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            auto v = make_test_view<CS, Tag, Sized>();
            AssertEqual<rah::sized_range<decltype(v)>, Sized>();
            return rah::views::transform(
                make_test_view<CS, Tag, Sized>(), [](auto a) { return a % 2 == 0; });
        }
        static constexpr bool is_sized = Sized;
        static constexpr bool is_common = rah::common_range<test_view<CS, Tag, Sized>>;
        static constexpr bool do_test = true;
    };
};
void test_transform_view()
{
    // Test transform
    {
        /// [rah::views::transform]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : rah::views::transform(vec, [](auto a) { return a * 2; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [rah::views::transform]
    }
    {
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        auto valueSelector = [](auto a)
        {
            return a * 2;
        };
        auto selectedValuesRange = rah::views::transform(vec, valueSelector);
        auto bounds =
            std::minmax_element(rah::begin(selectedValuesRange), rah::end(selectedValuesRange));
        auto min = *bounds.first;
        assert(min == 0);
        auto max = *bounds.second;
        assert(max == 6); // 3 * 2
    }
    {
        /// [rah::views::transform_pipeable]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : vec | rah::views::transform([](auto a) { return a * 2; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [rah::views::transform_pipeable]
    }

    check_all_cat<rah::random_access_iterator_tag, std::input_iterator_tag, make_transform_view::Trait>();
}

struct make_take_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::take(make_test_view<CS, Tag, Sized>(), 8);
        }
        static constexpr bool is_sized = Sized;
        using R = test_view<CS, Tag, Sized>;
        static constexpr bool is_common = rah::sized_range<R> && rah::random_access_range<R>;
        static constexpr bool do_test = true;
    };
};
void test_take_view()
{
    {
        /// [take]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::take(in, 5);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        auto range2 = rah::views::take(in, 1000);
        std::vector<int> out2;
        std::copy(rah::begin(range2), rah::end(range2), std::back_inserter(out2));
        assert(out2 == std::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        /// [take]
    }

    {
        /// [take_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | rah::views::take(5);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        auto range2 = in | rah::views::take(1000);
        std::vector<int> out2;
        std::copy(rah::begin(range2), rah::end(range2), std::back_inserter(out2));
        assert(out2 == std::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        /// [take_pipeable]
    }

    check_all_cat<rah::contiguous_iterator_tag, std::input_iterator_tag, make_take_view::Trait>();
}

struct make_drop_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::drop(make_test_view<CS, Tag, Sized>(), 2);
        }
        static constexpr bool is_sized = Sized;
        static constexpr bool is_common = rah::common_range<test_view<CS, Tag, Sized>>;
        static constexpr bool do_test = true;
    };
};
void test_drop_view()
{
    {
        /// [drop]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::drop(in, 6);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop]
    }

    {
        /// [drop_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | rah::views::drop(6);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop_pipeable]
    }

    check_all_cat<rah::contiguous_iterator_tag, std::input_iterator_tag, make_drop_view::Trait>();
}

struct make_drop_while_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::drop_while(
                make_test_view<CS, Tag, Sized>(), [](auto i) { return i < 4; });
        }
        using V = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized =
            rah::forward_range<V> && rah::sized_sentinel_for<rah::sentinel_t<V>, rah::iterator_t<V>>;
        static constexpr bool is_common = rah::common_range<V>;
        static constexpr bool do_test = true;
    };
};
void test_drop_while_view()
{
    {
        /// [drop_while]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::drop_while(in, [](auto v) { return v < 6; });
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop_while]
    }

    {
        /// [drop_while_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | rah::views::drop_while([](auto v) { return v < 6; });
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop_while_pipeable]
    }

    check_all_cat<rah::contiguous_iterator_tag, std::input_iterator_tag, make_drop_while_view::Trait>();
}

struct make_join_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::join(rah::views::transform(
                make_test_view<CS, Tag, Sized>(), [](auto i) { return rah::views::iota(0, i); }));
        }
        static constexpr bool is_sized = false;
        static constexpr bool is_common = false;
        static constexpr bool do_test = true;
    };
};
void test_join_view()
{
    {
        /// [join]
        std::vector<std::vector<int>> in = {
            {},
            {0, 1},
            {},
            {2, 3, 4},
            {5},
            {},
        };
        auto range = rah::views::join(in);
        std::vector<int> result;
        rah::copy(range, std::back_inserter(result));
        assert(result == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [join]
        // concepts :
        // TODO : Allow join to be bidirectional
        // OUTER and INNER are bidir => bidir
        auto bidir = rah::views::join(
            rah::views::repeat(test_view<Common, std::bidirectional_iterator_tag, false>(0, 10, 1)));
        // else if OUTER and INNER are forward => forward
        {
            auto forw = rah::views::join(
                rah::views::repeat(test_view<Common, std::forward_iterator_tag, false>(0, 10, 1)));
            using forwIterCateg = rah::details::iterator_category<rah::iterator_t<decltype(forw)>>;
            static_assert(std::is_same_v<forwIterCateg, std::input_iterator_tag>, "");
            STATIC_ASSERT((rah::input_range<decltype(forw)>));
            static_assert(not rah::bidirectional_range<decltype(forw)>, "");
        }
        {
            auto forw = rah::views::join(
                rah::views::repeat(test_view<Sentinel, std::forward_iterator_tag, false>(0, 10, 1)));
            using forwIterCateg = rah::details::iterator_category<rah::iterator_t<decltype(forw)>>;
            static_assert(std::is_same_v<forwIterCateg, std::input_iterator_tag>, "");
            static_assert(rah::input_range<decltype(forw)>, "");
            static_assert(not rah::bidirectional_range<decltype(forw)>, "");
        }
        // else if OUTER and INNER are input => input
        auto inputRange = rah::views::join(
            rah::views::repeat(test_view<Sentinel, std::input_iterator_tag, false>(0, 10, 1)));
        static_assert(rah::input_range<decltype(inputRange)>, "");
    }
    {
        // Test join on a range of rvalue
        auto range =
            rah::views::iota(0, 6)
            | rah::views::transform([](int i) { return rah::views::repeat(1) | rah::views::take(i); })
            | rah::views::join();
        std::vector<int> result;
        rah::copy(range, std::back_inserter(result));
        assert(result == std::vector<int>(15, 1));
    }

    {
        /// [join_pipeable]
        std::vector<std::vector<int>> in = {
            {0, 1},
            {},
            {2, 3, 4},
            {5},
            {},
        };
        auto range = in | rah::views::join();
        std::vector<int> result;
        rah::copy(range, std::back_inserter(result));
        assert(result == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [join_pipeable]
    }

    check_all_cat<rah::input_iterator_tag, std::input_iterator_tag, make_join_view::Trait>();
}

struct make_split_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        std::array<int, 2> delim = {3, 4};
        auto make()
        {
            return rah::views::split(make_test_view<CS, Tag, Sized>(), delim);
        }
        static constexpr bool is_sized = false;
        static constexpr bool is_common = false;
        static constexpr bool do_test = true;
    };
};
void test_split_view()
{
    /// [views::split]
    std::string sentence{"Keep..moving..forward.."};
    std::string delim{".."};
    auto words =
        rah::views::split(sentence, delim)
        | rah::views::transform([](auto word) { return std::string(word.begin(), word.end()); });

    EQUAL_RANGE(words, std::vector<std::string>({"Keep", "moving", "forward"}));
    for (auto&& word : words | rah::views::common())
        std::cout << std::string(word.begin(), word.end()) << ' ';
    /// [views::split]

    // TODO : Allow forward_iterator
    // TODO : Allow common_range
    // TODO : Check inner_range (reference_t)
    check_all_cat<rah::input_iterator_tag, std::input_iterator_tag, make_split_view::Trait>();
}

struct make_counted_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        std::array<int, 2> delim = {3, 4};
        auto make()
        {
            auto r = make_test_view<CS, Tag, Sized>();
            return rah::views::counted(r.begin(), 8);
        }
        using BaseRange = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized = rah::random_access_range<BaseRange>;
        static constexpr bool is_common = rah::random_access_iterator<rah::iterator_t<BaseRange>>;
        static constexpr bool do_test = true;
    };
};
void test_counted_view()
{
    /// [counted]
    std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    auto range = rah::views::counted(in.begin(), 5);
    std::vector<int> out;
    std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
    assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    /// [counted]

    check_all_cat<rah::contiguous_iterator_tag, std::input_iterator_tag, make_counted_view::Trait>();
}

struct make_common_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::common(make_test_view<CS, Tag, Sized>());
        }
        using BaseRange = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized = rah::sized_range<BaseRange>;
        static constexpr bool is_common = true;
        static constexpr bool do_test = true;
    };
};
void test_common_view()
{
    /// [rah::views::common]
    auto c = rah::views::iota(0, 5) | rah::views::filter([](auto i) { return i % 2 == 0; });
    std::vector<int> result;
    for (auto&& i : c | rah::views::common())
        result.push_back(i);
    assert(result == std::vector<int>({0, 2, 4}));
    /// [rah::views::common]

    check_all_cat<rah::forward_iterator_tag, rah::forward_iterator_tag, make_common_view::Trait>();
}

struct make_reverse_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::reverse(make_test_view<CS, Tag, Sized>());
        }
        using BaseRange = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized =
            rah::sized_range<BaseRange> || rah::random_access_iterator<rah::iterator_t<BaseRange>>;
        static constexpr bool is_common = true;
        static constexpr bool do_test = rah::bidirectional_range<BaseRange>;
    };
};
void test_reverse_iterator()
{
    {
        /// [reverse]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : rah::views::reverse(vec))
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
        /// [reverse]
    }
    {
        /// [reverse_pipeable]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : vec | rah::views::reverse())
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
        /// [reverse_pipeable]
    }

    check_all_cat<rah::random_access_iterator_tag, rah::input_iterator_tag, make_reverse_view::Trait>();
}

struct make_elements_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };

        auto make()
        {
            return rah::views::elements<2>(make_test_view_adapter<CS, Tag, Sized>(vec));
        }
        using BaseRange = test_view_adapter<CS, Tag, Sized, std::vector<std::tuple<bool, char, int>>>;
        static constexpr bool is_sized = rah::sized_range<BaseRange>;
        static constexpr bool is_common = rah::common_range<BaseRange>;
        static constexpr bool do_test = true;
    };
};
void test_elements_view()
{
    {
        /// [elements_view]
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        std::vector<int> result;
        for (auto i : vec | rah::views::elements<2>())
            result.push_back(i);
        assert(result == std::vector<int>({1000, 1001, 1002, 1003}));
        /// [elements_view]
    }
    {

        /// [values_view]
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        std::vector<char> result;
        for (auto i : vec | rah::views::values())
            result.push_back(i);
        assert(result == std::vector<char>({'a', 'b', 'c', 'd'}));
        /// [values_view]
    }
    {
        /// [keys_view]
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        std::vector<bool> result;
        for (auto i : vec | rah::views::keys())
            result.push_back(i);
        assert(result == std::vector<bool>({true, false, true, false}));
        /// [keys_view]
    }

    check_all_cat<rah::random_access_iterator_tag, std::input_iterator_tag, make_elements_view::Trait>();
}

struct make_enumerate_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::enumerate(make_test_view<CS, Tag, Sized>());
        }
        using BaseRange = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized = rah::sized_range<BaseRange>;
        static constexpr bool is_common = rah::common_range<BaseRange> && rah::sized_range<BaseRange>;
        static constexpr bool do_test = true;
    };
};
void test_enumerate_view()
{
    {
        /// [enumerate]
        std::vector<int> input{4, 5, 6, 7};
        std::vector<std::tuple<size_t, int>> result;
        auto toto = rah::views::enumerate(input);
        auto prout = toto.end();
        for (auto i_value : rah::views::enumerate(input))
            result.emplace_back(i_value);
        assert(result == (std::vector<std::tuple<size_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate]
    }
    {
        /// [enumerate_pipeable]
        std::vector<int> input{4, 5, 6, 7};
        std::vector<std::tuple<size_t, int>> result;
        for (auto i_value : input | rah::views::enumerate() | rah::views::common())
            result.emplace_back(i_value);
        assert(result == (std::vector<std::tuple<size_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate_pipeable]
    }

    {
        // This can't work since enumerate return an rvalue pairs since map_key want an lvalue
        bool bools[] = {false, true, true, false, false, true};
        auto range = bools | rah::views::enumerate()
                     | rah::views::filter([](auto&& index_bool) { return std::get<1>(index_bool); })
                     | rah::views::keys();

        std::vector<size_t> ref;
        rah::copy(range, std::back_inserter(ref));
        assert(ref == (std::vector<size_t>{1, 2, 5}));
    }

    check_all_cat<rah::random_access_iterator_tag, std::input_iterator_tag, make_enumerate_view::Trait>();
}

struct make_zip_view1
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::zip(make_test_view<CS, Tag, Sized>());
        }
        using BaseRange = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized = rah::sized_range<BaseRange>;
        static constexpr bool is_common =
            rah::common_range<BaseRange>
            || (rah::sized_range<BaseRange> && rah::random_access_range<BaseRange>);
        static constexpr bool do_test = true;
    };
};
struct make_zip_view2
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::zip(
                make_test_view<CS, Tag, Sized>(),
                make_test_view<Common, rah::contiguous_iterator_tag, true>());
        }
        using BaseRange1 = test_view<CS, Tag, Sized>;
        using BaseRange2 = test_view<Common, rah::contiguous_iterator_tag, true>;
        static constexpr bool is_sized = rah::sized_range<BaseRange1> && rah::sized_range<BaseRange2>;
        static constexpr bool is_common =
            (rah::sized_range<BaseRange1> && rah::random_access_range<BaseRange1>)&&(
                rah::sized_range<BaseRange2> && rah::random_access_range<BaseRange2>);
        static constexpr bool do_test = true;
    };
};
void test_zip_view()
{
    {
        /// [zip]
        std::vector<int> inputA{1, 2, 3, 4};
        std::vector<double> inputB{2.5, 4.5, 6.5, 8.5};
        std::vector<char> inputC{'a', 'b', 'c', 'd', 'e', 'f', 'g'};
        std::vector<std::tuple<int, double, char>> result;
        for (auto a_b_c : rah::views::zip(inputA, inputB, inputC) | rah::views::common())
            result.emplace_back(a_b_c);
        assert(
            result
            == (std::vector<std::tuple<int, double, char>>{
                {1, 2.5, 'a'}, {2, 4.5, 'b'}, {3, 6.5, 'c'}, {4, 8.5, 'd'}}));
        /// [zip]
    }

    {
        std::vector<int> inputA{1, 2, 3, 4};
        std::vector<bool> inputB{false, true, true, false};
        auto range = rah::views::zip(inputA, inputB)
                     | rah::views::filter([](auto a_b) { return std::get<1>(a_b); });
        std::vector<std::tuple<int, bool>> result;

        rah::copy(range, std::back_inserter(result));
        assert(rah::equal(result, std::vector<std::tuple<int, bool>>({{2, true}, {3, true}})));
    }

    check_all_cat<rah::random_access_iterator_tag, std::input_iterator_tag, make_zip_view1::Trait>();
    check_all_cat<rah::random_access_iterator_tag, std::input_iterator_tag, make_zip_view2::Trait>();
}

struct make_adjacent_view
{
    template <CommonOrSent CS, typename Tag, bool Sized>
    struct Trait
    {
        auto make()
        {
            return rah::views::adjacent<3>(make_test_view<CS, Tag, Sized>());
        }
        using BaseRange = test_view<CS, Tag, Sized>;
        static constexpr bool is_sized = rah::sized_range<BaseRange>;
        static constexpr bool is_common = rah::common_range<BaseRange>;
        static constexpr bool do_test = true;
    };
};
void test_adjacent_view()
{
    {
        /// [adjacent]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah::views::adjacent<3>(in))
        {
            out.push_back({std::get<0>(abc), std::get<1>(abc), std::get<2>(abc)});
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [adjacent]
    }
    {
        // adjacent With non common_range
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah::views::iota(0) | rah::views::take(6) | rah::views::adjacent<3>())
        {
            out.push_back({std::get<0>(abc), std::get<1>(abc), std::get<2>(abc)});
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }
    {
        // adjacent With N > view.size()
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah::views::iota(0) | rah::views::take(6) | rah::views::adjacent<45>())
        {
            out.push_back({std::get<0>(abc), std::get<1>(abc), std::get<2>(abc)});
        }
        assert(out == (std::vector<std::vector<int>>{}));
    }
    {
        // adjacent With N == 0
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah::views::iota(0) | rah::views::take(6) | rah::views::adjacent<0>())
        {
            static_assert(
                std::tuple_size_v<std::remove_reference_t<decltype(abc)>> == 0,
                "tuple should be empty");
            out.emplace_back();
        }
        assert(out.empty());
    }

    check_all_cat<rah::random_access_iterator_tag, std::forward_iterator_tag, make_adjacent_view::Trait>();
}

int main()
{
    test_counted_iterator();
    test_empty_view();
    test_single_view();
    test_iota_view();
    test_istream_view();
    test_repeat_view();
    test_owning_view();
    test_all_view();
    test_filter_view();
    test_transform_view();
    test_take_view();
    test_drop_view();
    test_drop_while_view();
    test_join_view();
    test_split_view();
    test_counted_view();
    test_reverse_iterator();
    test_elements_view();
    test_enumerate_view();
    test_zip_view();
    test_adjacent_view();

    {
        std::vector<int> vec{0, 1, 2, 2, 3};
        std::vector<int> out;
        auto ref = vec | rah::views::ref();
        for (auto&& val : ref)
        {
            out.push_back(val);
        }
        assert(out == (std::vector<int>{0, 1, 2, 2, 3}));
    }

    {
        std::vector<int> vec{0, 1, 2, 2, 3};
        toto(vec);
        toto({0, 1, 2, 2, 3});
    }
    {
        /// [make_pipeable use]
        std::vector<int> vec{0, 1, 2, 2, 3};
        assert((vec | test_count(2)) == 2);
        /// [make_pipeable use]
    }

    // *********************************** views **************************************************

    {
        /// [irange]
        std::vector<int> result;
        for (int i : rah::views::irange(10, 19, 2))
            result.push_back(i);
        assert(result == std::vector<int>({10, 12, 14, 16, 18}));
        /// [irange]
        STATIC_ASSERT((rah::random_access_range<decltype(rah::views::irange(10, 19, 2))>));
    }

    {
        std::vector<int> result;
        for (int i : rah::views::irange(-5, 5, 2))
            result.push_back(i);
        assert(result == std::vector<int>({-5, -3, -1, 1, 3}));
    }

    {
        std::vector<int> result;
        for (int i : rah::views::irange(-15, -6, 2))
            result.push_back(i);
        assert(result == std::vector<int>({-15, -13, -11, -9, -7}));
    }

    {
        /// [for_each]
        auto createRange = [](int i)
        {
            return rah::views::repeat(char('a' + i)) | rah::views::take(i);
        };
        auto range = rah::views::for_each(rah::views::iota(0, 5), createRange);
        std::string result;
        rah::copy(range, std::back_inserter(result));
        assert(result == "bccdddeeee");
        /// [for_each]
    }

    {
        /// [for_each_pipeable]
        auto range =
            rah::views::iota(0, 3)
            | rah::views::for_each(
                [&](int z)
                {
                    return rah::views::iota(3, 6)
                           | rah::views::for_each(
                               [&, z](int y)
                               {
                                   return rah::views::iota(6, 9)
                                          | rah::views::for_each(
                                              [&, y, z](int x)
                                              { return rah::views::single(x + y * 3 + z * 9); });
                               });
                });
        assert(equal(range, rah::views::iota(15, 42)));
        /// [for_each_pipeable]
    }

    {
        size_t count = 0;
        size_t count2 = 0;
        size_t count3 = 0;
        auto range =
            rah::views::iota(0, 3)
            | rah::views::for_each(
                [&](int z)
                {
                    ++count;
                    return rah::views::iota(3, 6)
                           | rah::views::for_each(
                               [&, z](int y)
                               {
                                   ++count2;
                                   return rah::views::iota(6, 9)
                                          | rah::views::for_each(
                                              [&, y, z](int x)
                                              {
                                                  ++count3;
                                                  return rah::views::single(x + y * 3 + z * 9);
                                              });
                               });
                });

        assert(equal(range, rah::views::iota(15, 42)));
        assert(count == 3);
        assert(count2 == 9);
        assert(count3 == 27);
    }

    {
        size_t xSize = 2;
        size_t ySize = 3;
        auto xyIndexes = [=](size_t y)
        {
            return rah::views::zip(rah::views::repeat(y), rah::views::iota<size_t>(0, xSize));
        };
        auto range = rah::views::iota<size_t>(0, ySize) | rah::views::for_each(xyIndexes);
        std::vector<std::tuple<size_t, size_t>> result;
        rah::copy(range, std::back_inserter(result));
        assert(
            result
            == (std::vector<std::tuple<size_t, size_t>>{
                {0, 0}, {0, 1}, {1, 0}, {1, 1}, {2, 0}, {2, 1}}));

        size_t zSize = 4;
        auto xyzIndexes = [=](size_t z)
        {
            return rah::views::zip(
                rah::views::repeat(z),
                rah::views::iota<size_t>(0, ySize) | rah::views::for_each(xyIndexes));
        };
        auto flattenTuple = [](auto&& z_yx)
        {
            using namespace std;
            return std::make_tuple(get<0>(z_yx), get<0>(get<1>(z_yx)), get<1>(get<1>(z_yx)));
        };
        auto rangeZYX = rah::views::iota<size_t>(0, zSize) | rah::views::for_each(xyzIndexes)
                        | rah::views::transform(flattenTuple);
        std::vector<std::tuple<size_t, size_t, size_t>> resultZYX;
        rah::copy(rangeZYX, std::back_inserter(resultZYX));
        assert(
            resultZYX
            == (std::vector<std::tuple<size_t, size_t, size_t>>{
                {0, 0, 0}, {0, 0, 1}, {0, 1, 0}, {0, 1, 1}, {0, 2, 0}, {0, 2, 1},
                {1, 0, 0}, {1, 0, 1}, {1, 1, 0}, {1, 1, 1}, {1, 2, 0}, {1, 2, 1},
                {2, 0, 0}, {2, 0, 1}, {2, 1, 0}, {2, 1, 1}, {2, 2, 0}, {2, 2, 1},
                {3, 0, 0}, {3, 0, 1}, {3, 1, 0}, {3, 1, 1}, {3, 2, 0}, {3, 2, 1}}));
    }

    {
        /// [generate]
        int y = 1;
        auto gen = rah::views::generate(
            [&y]() mutable
            {
                auto prev = y;
                y *= 2;
                return prev;
            });
        std::vector<int> gen_copy;
        std::copy_n(begin(gen), 4, std::back_inserter(gen_copy));
        assert(gen_copy == std::vector<int>({1, 2, 4, 8}));
        /// [generate]
        static_assert(rah::input_range<decltype(gen)>, "");
        static_assert(
            std::is_same_v<rah::range_iter_categ_t<decltype(gen)>, std::input_iterator_tag>, "");
        static_assert(not rah::forward_range<decltype(gen)>, "");
        static_assert(not rah::common_range<decltype(gen)>, "");
    }
    {
        /// [generate_n]
        std::vector<int> result;
        int y = 1;
        auto gen = rah::views::generate_n(
            4,
            [&y]() mutable
            {
                auto prev = y;
                y *= 2;
                return prev;
            });
        auto i = rah::begin(gen);
        auto e = rah::end(gen);
        for (; i != e; ++i)
            result.push_back(*i);
        assert(result == std::vector<int>({1, 2, 4, 8}));
        /// [generate_n]
        static_assert(rah::input_range<decltype(gen)>, "");
        static_assert(
            std::is_same_v<rah::range_iter_categ_t<decltype(gen)>, std::input_iterator_tag>, "");
        static_assert(not rah::forward_range<decltype(gen)>, "");
        static_assert(not rah::common_range<decltype(gen)>, "");
    }

    {
        /// [cycle]
        std::vector<int> in{0, 1, 2};
        auto cy = rah::views::cycle(in);
        std::vector<int> out;
        std::copy_n(cy.begin(), 8, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle]
        STATIC_ASSERT((rah::bidirectional_range<decltype(cy)>));
        STATIC_ASSERT((not rah::random_access_range<decltype(cy)>));
    }
    { // Cycle + input/sentinel => input/sentinel
        auto cyInputSent =
            rah::views::cycle(make_test_view<Sentinel, std::input_iterator_tag, false>());
        STATIC_ASSERT(is_input_not_common<decltype(cyInputSent)>);
    }
    {
        auto cyForwardSent =
            rah::views::cycle(make_test_view<Sentinel, std::forward_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyForwardSent)>);
    }
    {
        auto cyForwardCommon =
            rah::views::cycle(make_test_view<Common, std::forward_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyForwardCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyBidirSent =
            rah::views::cycle(make_test_view<Sentinel, std::bidirectional_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyBidirSent)>);
    }
    {
        auto cyBidirCommon =
            rah::views::cycle(make_test_view<Common, std::bidirectional_iterator_tag, false>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyBidirCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyRandomSent =
            rah::views::cycle(make_test_view<Sentinel, std::random_access_iterator_tag, true>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyRandomSent)>);
    }
    {
        auto cyRandomCommon =
            rah::views::cycle(make_test_view<Common, std::random_access_iterator_tag, true>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyRandomCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyContiSent =
            rah::views::cycle(make_test_view<Sentinel, rah::contiguous_iterator_tag, true>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyContiSent)>);
    }
    {
        auto cyContiCommon =
            rah::views::cycle(make_test_view<Common, rah::contiguous_iterator_tag, true>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyContiCommon)>);
    }

    {
        /// [cycle_pipeable]
        std::vector<int> in{0, 1, 2};
        auto cy = in | rah::views::cycle();
        std::vector<int> out;
        std::copy_n(cy.begin(), 8, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle_pipeable]
    }

    {
        std::vector<int> in{0, 1, 2};
        auto cy = rah::views::cycle(in) | rah::views::take(8);
        std::vector<int> out;
        // static_assert(RAH_NAMESPACE::range<decltype(std::back_inserter(out))>, "dkjh");
        rah::copy(cy, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
    }

    {
        std::vector<int> in{0, 1, 2};
        auto range = rah::views::drop(in, 6);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out.empty());
    }

    {
        /// [sliding]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah::views::slide(in, 3))
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding]
    }
    {
        // slide with non common_range
        std::vector<std::vector<int>> out;
        auto r = rah::views::iota(0) | rah::views::take(6) | rah::views::slide(3);
        auto it = rah::begin(r);
        auto e = rah::end(r);
        for (; it != e; ++it)
        {
            out.emplace_back();
            auto&& subRange = *it;
            rah::copy(subRange, std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }

    {
        /// [adjacent_transform]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<int> out;
        for (auto abc :
             rah::views::adjacent_transform<3>(in, [](auto a, auto b, auto c) { return a + b + c; }))
        {
            out.push_back(abc);
        }
        assert(out == (std::vector<int>{3, 6, 9, 12}));
        /// [adjacent_transform]
    }
    {
        // adjacent_transform With non common_range
        std::vector<int> out;
        for (auto abc : rah::views::iota(0) | rah::views::take(6)
                            | rah::views::adjacent_transform<3>([](auto a, auto b, auto c)
                                                                { return a + b + c; }))
        {
            out.push_back(abc);
        }
        assert(out == (std::vector<int>{3, 6, 9, 12}));
    }
    {
        // adjacent_transform With N > view.size()
        std::vector<int> out;
        for (auto abc :
             rah::views::iota(0) | rah::views::take(6) | rah::views::adjacent_transform<45>(Add{}))
        {
            out.push_back(abc);
        }
        assert(out == (std::vector<int>{}));
    }
    {
        // adjacent_transform With N == 0
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah::views::iota(0) | rah::views::take(6)
                              | rah::views::adjacent_transform<0>([](auto i) { return i + 1; }))
        {
            static_assert(
                std::tuple_size_v<std::remove_reference_t<decltype(abc)>> == 0,
                "tuple should be empty");
            out.push_back({});
        }
        assert(out == (std::vector<std::vector<int>>{}));
    }
    {
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        auto range = in | rah::views::cycle() | rah::views::slide(3) | rah::views::take(in.size());
        for (auto subRange : range | rah::views::common())
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(
            out
            == (std::vector<std::vector<int>>{
                {0, 1, 2},
                {1, 2, 3},
                {2, 3, 4},
                {3, 4, 5},
                {4, 5, 0},
                {5, 0, 1},
            }));
    }

    {
        /// [sliding_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        for (auto subRange : in | rah::views::slide(3))
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding_pipeable]
    }

    {
        std::vector<int> in{0, 1, 2, 3};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah::views::slide(in, 4))
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2, 3}}));
    }

    {
        std::vector<int> in{0, 1};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah::views::slide(in, 4))
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{}));
    }

    {
        std::vector<int> in{0, 1, 2, 3};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah::views::slide(in, 0))
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0}, {1}, {2}, {3}}));
    }

    {
        std::vector<int> in{0, 1, 2, 3};
        std::vector<std::vector<int>> out;
        for (auto subRange : in | rah::views::slide(1))
        {
            out.emplace_back();
            std::copy(rah::begin(subRange), rah::end(subRange), std::back_inserter(out.back()));
        }
        assert(
            out
            == (std::vector<std::vector<int>>{
                {0},
                {1},
                {2},
                {3},
            }));
    }

    {
        /// [unbounded]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::unbounded(in.begin());
        std::vector<int> out;
        std::copy_n(rah::begin(range), 5, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        /// [unbounded]
    }

    {
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::unbounded(in.begin()) | rah::views::slice(0, 5);
        std::vector<int> out;
        rah::copy(range, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        int in[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range =
            rah::views::unbounded((int const* const)std::begin(in)) | rah::views::slice(0, 5);
        std::vector<int> out;
        rah::copy(range, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        /// [counted_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        auto range = in | rah::views::take(9);
        std::vector<int> out;
        auto a = rah::begin(range);
        auto b = rah::end(range);
        volatile auto dist = std::distance(a, b);
        (void)dist;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [counted_pipeable]
    }

    {
        /// [slice]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : rah::views::slice(vec, 2, 6))
            result.push_back(i);
        assert(result == std::vector<int>({2, 3, 4, 5}));
        std::vector<int> result2;
        for (int i : rah::views::slice(vec, 2, 6))
            result2.push_back(i);
        assert(result2 == std::vector<int>({2, 3, 4, 5}));
        /// [slice]
    }
    {
        /// [slice_pipeable]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : vec | rah::views::slice(2, 6))
            result.push_back(i);
        assert(result == std::vector<int>({2, 3, 4, 5}));
        /// [slice_pipeable]
    }

    {
        /// [stride]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : rah::views::stride(vec, 2))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [stride]
    }
    {
        /// [stride_pipeable]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : vec | rah::views::stride(2))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [stride_pipeable]
    }

    {
        // Pass a rvalue container to a views is possible thought a owning_view
        auto getVec = []
        {
            return std::vector<int>{0, 1, 2, 3};
        };
        std::vector<int> result;
        for (int i : rah::views::reverse(getVec()))
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
    }

    {
        /// [zip_transform]
        std::vector<int> inputA{1, 2, 3, 4};
        std::vector<double> inputB{2.5, 4.5, 6.5, 8.5};
        std::vector<char> inputC{'a', 'b', 'c', 'd', 'e', 'f', 'g'};
        std::vector<std::string> result;
        auto func = [](int a, double d, char c)
        {
            std::stringstream ss;
            ss << a << d << c;
            return ss.str();
        };
        for (auto a_b_c :
             rah::views::zip_transform(func, inputA, inputB, inputC) | rah::views::common())
            result.emplace_back(a_b_c);
        assert(result == (std::vector<std::string>{{"12.5a"}, {"24.5b"}, {"36.5c"}, {"48.5d"}}));
        /// [zip_transform]
    }

    {
        /// [chunk]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<std::vector<int>> result;
        for (auto elts : rah::views::chunk(vec_01234, 2))
            result.emplace_back(rah::begin(elts), rah::end(elts));
        assert(result == std::vector<std::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk]
    }
    {
        /// [chunk_pipeable]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<std::vector<int>> result;
        for (auto elts : vec_01234 | rah::views::chunk(2))
            result.emplace_back(rah::begin(elts), rah::end(elts));
        assert(result == std::vector<std::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk_pipeable]
    }
    {
        /// Chunk with non-common_view
        auto vec_01234 = rah::views::iota(0) | rah::views::take(5);
        std::vector<std::vector<int>> result;
        for (auto elts : rah::views::chunk(vec_01234, 2))
            result.emplace_back(rah::begin(elts), rah::end(elts));
        assert(result == std::vector<std::vector<int>>({{0, 1}, {2, 3}, {4}}));
    }

    {
        auto range = rah::views::generate_n(5, []() { return rand(); })
                     | rah::views::filter([](auto&& val) { return val % 2 == 0; });
        std::vector<int> result;
        for (int i : range)
            result.push_back(i);
    }
    {
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : rah::views::filter(vec_01234, &is_odd))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
    }
    {
        enum class Tutu
        {
            a,
            b,
            c,
            d,
            e
        };
        std::vector<Tutu> vec_01234{Tutu::a, Tutu::b, Tutu::c, Tutu::d, Tutu::e};
        std::vector<Tutu> result;
        for (Tutu i : rah::views::filter(vec_01234, [](Tutu a) { return a != Tutu::c; }))
            result.push_back(i);
        assert(result == std::vector<Tutu>({Tutu::a, Tutu::b, Tutu::d, Tutu::e}));
    }

    {
        int vec_01234[] = {0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : rah::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
    }
    {
        std::vector<std::vector<int>> vec_01234 = {
            {0},
            {1},
            {2},
            {3},
            {4},
        };
        std::vector<bool> vec_bool = {
            true,
            true,
            true,
            true,
            true,
        };
        std::vector<std::vector<int>> result;
        for (auto&& i :
             rah::views::zip(vec_01234, vec_bool)
                 | rah::views::filter([](auto&& a) { return std::get<0>(a).front() % 2 == 0; })
                 | rah::views::common())
            result.push_back(std::get<0>(i));
        assert(result == (std::vector<std::vector<int>>{{0}, {2}, {4}}));
        assert(vec_01234 == (std::vector<std::vector<int>>{{0}, {1}, {2}, {3}, {4}}));
    }
    {
        /// [filter_pipeable]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : vec_01234 | rah::views::filter([](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
        /// [filter_pipeable]
    }

    {
        // test filter with the first elements filtered
        auto range1 =
            rah::views::iota(1, 10) | rah::views::filter([](auto&& val) { return val % 2 == 0; });
        assert(rah::none_of(range1, [](auto v) { return (v % 2) == 1; }));

        // test generate + filter
        auto range2 = rah::views::generate_n(100, []() { return rand(); })
                      | rah::views::filter([](auto&& val) { return val % 2 == 0; });

        assert(rah::none_of(range2, [](auto v) { return (v % 2) == 1; }));

        // Can create some compilation issue about lambda copy
        auto range3 = rah::views::iota(0, 5)
                      | rah::views::for_each(
                          [](auto)
                          {
                              return rah::views::generate_n(5, []() { return rand(); })
                                     | rah::views::filter([](auto&& val) { return val % 2 == 0; });
                          });
        assert(rah::none_of(range3, [](auto v) { return (v % 2) == 1; }));
    }

    {
        /// [concat]
        std::vector<int> inputA{0, 1, 2, 3};
        std::vector<int> inputB{4, 5, 6};
        std::vector<int> inputC{7, 8, 9, 10, 11};
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3, 4, 5, 6}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB, inputC))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}));
        }
        /// [concat]
    }
    {
        std::vector<int> inputA{};
        std::vector<int> inputB{1, 2, 3, 4};
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({1, 2, 3, 4}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({1, 2, 3, 4}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputA))
                result.push_back(i);
            assert(result == std::vector<int>({}));
        }
    }

    {
        /// [views::set_difference]
        std::vector<int> in1 = {1, 2, 3, 4, 5, 6};
        std::vector<int> in2 = {2, 4, 6, 7, 8, 9, 10};
        std::vector<int> out;
        for (int val : rah::views::set_difference(in1, in2))
            out.push_back(val);
        assert(out == std::vector<int>({1, 3, 5}));
        /// [views::set_difference]
    }

    {
        /// views::set_difference
        auto test_set_difference = [](std::vector<int> const& in1,
                                      std::vector<int> const& in2,
                                      std::vector<int> const& expected)
        {
            std::vector<int> out;
            for (int val : rah::views::set_difference(in1, in2))
                out.push_back(val);
            assert(out == expected);
        };

        test_set_difference({}, {2, 4, 6, 7, 8, 9, 10}, {});
        test_set_difference({1, 2, 3, 4, 5, 6}, {}, {1, 2, 3, 4, 5, 6});
        test_set_difference({1, 2, 3, 4, 5, 6, 7}, {2, 4, 6}, {1, 3, 5, 7});
        test_set_difference({1, 2, 4, 6}, {3, 5, 7}, {1, 2, 4, 6});
        test_set_difference({1, 2, 4, 6}, {1, 2, 4, 6}, {});
        test_set_difference({1, 2, 4, 6, 7, 8, 9}, {1, 2, 4, 6}, {7, 8, 9});

        for (int x = 0; x < 100; ++x)
        {
            std::vector<int> in1;
            std::vector<int> in2;
            size_t const size1 = rand() % 100;
            size_t const size2 = rand() % 100;
            for (size_t i = 0; i < size1; ++i)
                in1.push_back(rand() % 100);
            for (size_t i = 0; i < size2; ++i)
                in2.push_back(rand() % 100);
            rah::sort(in1);
            rah::sort(in2);
            std::vector<int> outRef;
            std::set_difference(
                begin(in1), end(in1), begin(in2), end(in2), std::back_inserter(outRef));
            std::vector<int> out;
            for (int val : in1 | rah::views::set_difference(in2))
                out.push_back(val);
            assert(out == outRef);
        }
    }

    // *********************************** algos **************************************************

    {
        /// [rah::equal_range]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 0))
                out.push_back(i);
            assert(out == std::vector<int>({}));
        }
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 1))
                out.push_back(i);
            assert(out == std::vector<int>({1}));
        }
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 2))
                out.push_back(i);
            assert(out == std::vector<int>({2, 2}));
        }
        /// [rah::equal_range]
    }
    {
        /// [rah::equal_range_pipeable]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        {
            std::vector<int> out;
            for (int i : vecIn1 | rah::equal_range(0))
                out.push_back(i);
            assert(out == std::vector<int>({}));
        }
        {
            std::vector<int> out;
            for (int i : vecIn1 | rah::equal_range(1))
                out.push_back(i);
            assert(out == std::vector<int>({1}));
        }
        {
            std::vector<int> out;
            for (int i : vecIn1 | rah::equal_range(2))
                out.push_back(i);
            assert(out == std::vector<int>({2, 2}));
        }
        /// [rah::equal_range_pipeable]
    }
    {
        /// [rah::equal_range_pred_0]
        struct S
        {
            int value;
            char test;
            bool operator==(S rhs) const
            {
                return value == rhs.value && test == rhs.test;
            }
        };
        struct FindS
        {
            bool operator()(S s, int val) const
            {
                return s.value < val;
            }
            bool operator()(int val, S s) const
            {
                return val < s.value;
            }
        };
        /// [rah::equal_range_pred_0]
        {
            /// [rah::equal_range_pred]
            std::vector<S> vecIn1{{1, 'a'}, {2, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'e'}};
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 0, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({}));
            }
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 1, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{1, 'a'}}));
            }
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 2, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{2, 'b'}, {2, 'c'}}));
            }
            /// [rah::equal_range_pred]
        }
        {
            /// [rah::equal_range_pred_pipeable]
            std::vector<S> vecIn1{{1, 'a'}, {2, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'e'}};
            {
                std::vector<S> out;
                for (S i : vecIn1 | rah::equal_range(0, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({}));
            }
            {
                std::vector<S> out;
                for (S i : vecIn1 | rah::equal_range(1, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{1, 'a'}}));
            }
            {
                std::vector<S> out;
                for (S i : vecIn1 | rah::equal_range(2, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{2, 'b'}, {2, 'c'}}));
            }
            /// [rah::equal_range_pred_pipeable]
        }
    }

    {
        /// [rah::binary_search]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        assert(not rah::binary_search(vecIn1, 0));
        assert(rah::binary_search(vecIn1, 1));
        assert(rah::binary_search(vecIn1, 2));
        /// [rah::binary_search]
    }
    {
        /// [rah::binary_search_pipeable]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        assert(not(vecIn1 | rah::binary_search(0)));
        assert(vecIn1 | rah::binary_search(1));
        assert(vecIn1 | rah::binary_search(2));
        /// [rah::binary_search_pipeable]
    }
    {
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecOut{0, 0, 0, 0};
        rah::transform(vecIn1, begin(vecOut), [](int a) { return a + 1; });
        assert(vecOut == std::vector<int>({1, 2, 3, 4}));
    }
    {
        /// [rah::transform3]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecOut;
        rah::transform(vecIn1, std::back_inserter(vecOut), [](int a) { return a + 1; });
        assert(vecOut == std::vector<int>({1, 2, 3, 4}));
        /// [rah::transform3]
    }
    {
        /// [rah::transform4]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecIn2{4, 3, 2, 1};
        std::vector<int> vecOut;
        rah::transform(
            vecIn1, vecIn2, std::back_inserter(vecOut), [](int a, int b) { return a + b; });
        assert(vecOut == std::vector<int>({4, 4, 4, 4}));
        /// [rah::transform4]
    }

    assert((rah::views::iota(0, 0) | rah::reduce(0, [](auto a, auto b) { return a + b; })) == 0);
    {
        /// [rah::reduce]
        std::vector<int> vecIn1{1, 2, 3, 4};
        assert(rah::reduce(vecIn1, 0, [](auto a, auto b) { return a + b; }) == 10);
        /// [rah::reduce]
    }
    {
        /// [rah::reduce_pipeable]
        std::vector<int> vecIn1{1, 2, 3, 4};
        assert((vecIn1 | rah::reduce(0, [](auto a, auto b) { return a + b; })) == 10);
        /// [rah::reduce_pipeable]
    }

    /// [rah::any_of]
    assert(rah::any_of(std::initializer_list<int>{3, 0, 1, 3, 4, 6}, [](auto a) { return a == 3; }));
    /// [rah::any_of]
    /// [rah::any_of_pipeable]
    assert(
        (std::initializer_list<int>{0, 1, 2, 3, 4, 6} | rah::any_of([](auto a) { return a == 3; })));
    /// [rah::any_of_pipeable]
    assert(
        (std::initializer_list<int>{3, 0, 1, 3, 4, 6} | rah::any_of([](auto a) { return a == 3; })));
    assert(
        (std::initializer_list<int>{2, 0, 1, 2, 4, 6} | rah::any_of([](auto a) { return a == 3; }))
        == false);

    /// [rah::all_of]
    assert(rah::all_of(std::initializer_list<int>{4, 4, 4, 4}, [](auto a) { return a == 4; }));
    /// [rah::all_of]
    assert(
        rah::all_of(std::initializer_list<int>{4, 4, 3, 4}, [](auto a) { return a == 4; }) == false);
    assert((std::initializer_list<int>{4, 4, 4, 4} | rah::all_of([](auto a) { return a == 4; })));
    /// [rah::all_of_pipeable]
    assert(
        (std::initializer_list<int>{4, 4, 3, 4} | rah::all_of([](auto a) { return a == 4; }))
        == false);
    /// [rah::all_of_pipeable]

    /// [rah::none_of]
    assert((rah::none_of(std::initializer_list<int>{7, 8, 9, 10}, [](auto a) { return a == 11; })));
    /// [rah::none_of]
    assert((std::initializer_list<int>{7, 8, 9, 10} | rah::none_of([](auto a) { return a == 11; })));
    /// [rah::none_of_pipeable]
    assert(
        (std::initializer_list<int>{7, 8, 9, 10, 11} | rah::none_of([](auto a) { return a == 11; }))
        == false);
    /// [rah::none_of_pipeable]

    /// [rah::count]
    assert(rah::count(std::initializer_list<int>{4, 4, 4, 3}, 3) == 1);
    /// [rah::count]
    /// [rah::count_pipeable]
    assert((std::initializer_list<int>{4, 4, 4, 3} | rah::count(4)) == 3);
    /// [rah::count_pipeable]

    /// [rah::count_if]
    assert(rah::count_if(std::initializer_list<int>{4, 4, 4, 3}, [](auto a) { return a == 4; }) == 3);
    /// [rah::count_if]
    /// [rah::count_if_pipeable]
    assert(
        (std::initializer_list<int>{4, 4, 4, 3} | rah::count_if([](auto a) { return a == 3; })) == 1);
    /// [rah::count_if_pipeable]

    {
        /// [rah::for_each]
        std::vector<int> testFE{4, 4, 4, 4};
        rah::for_each(testFE, [](auto& value) { return ++value; });
        EQUAL_RANGE(testFE, std::initializer_list<int>({5, 5, 5, 5}));
        /// [rah::for_each]
    }
    {
        /// [rah::for_each_pipeable]
        std::vector<int> testFE{4, 4, 4, 4};
        testFE | rah::for_each([](auto& value) { return ++value; });
        EQUAL_RANGE(testFE, std::initializer_list<int>({5, 5, 5, 5}));
        /// [rah::for_each_pipeable]
    }

    {
        /// [rah::to_container_pipeable]
        std::vector<std::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        std::map<int, char> map_4a_5b_6c_7d = in1 | rah::to<std::map<int, char>>();
        assert(map_4a_5b_6c_7d == (std::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        std::list<int> in2{4, 5, 6, 7};
        std::vector<int> out = in2 | rah::to<std::vector<int>>();
        assert(out == (std::vector<int>{4, 5, 6, 7}));
        /// [rah::to_container_pipeable]
    }
    {
        /// [rah::to]
        std::vector<std::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        std::map<int, char> map_4a_5b_6c_7d = rah::to<std::map<int, char>>(in1);
        assert(map_4a_5b_6c_7d == (std::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        std::list<int> in2{4, 5, 6, 7};
        std::vector<int> out = rah::to<std::vector<int>>(in2);
        assert(out == (std::vector<int>{4, 5, 6, 7}));
        /// [rah::to]
    }

    {
        /// [rah::mismatch]
        std::vector<int> in1 = {1, 2, 3, 4};
        std::vector<int> in2 = {1, 2, 42, 42};
        auto r1_r2 = rah::mismatch(in1, in2);
        std::vector<int> out1;
        std::vector<int> out2;
        std::copy(std::get<0>(r1_r2), end(in1), std::back_inserter(out1));
        std::copy(std::get<1>(r1_r2), end(in2), std::back_inserter(out2));
        assert(out1 == std::vector<int>({3, 4}));
        assert(out2 == std::vector<int>({42, 42}));
        /// [rah::mismatch]
    }

    {
        /// [rah::find]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = rah::find(in, 3);
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find]
    }
    {
        /// [rah::find_pipeable]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = in | rah::find(3);
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find_pipeable]
    }

    /*{
        // rah::find_pipeable on non copyable type
        struct NonCopyable
        {
            NonCopyable() = default;
            NonCopyable(NonCopyable const&) = delete;
            NonCopyable& operator=(NonCopyable const&) = delete;
            NonCopyable(NonCopyable&&) = default;
            NonCopyable& operator=(NonCopyable&&) = default;
            int value;

            bool operator==(NonCopyable const& rho)
            {
                return value == rho.value;
            }
        };
        std::vector<NonCopyable> in;
        in.emplace_back(NonCopyable{ 1 });
        in.emplace_back(NonCopyable{ 2 });
        in.emplace_back(NonCopyable{ 3 });
        in.emplace_back(NonCopyable{ 4 });
        auto iter = in | rah::find(NonCopyable{ 3 });
        assert(
            (rah::make_subrange(iter, end(in))
             | rah::equal(std::initializer_list<NonCopyable>({ { 3 }, { 4 } }))));
    }*/
    {
        /// [rah::find_if]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = rah::find_if(in, [](int i) { return i == 3; });
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find_if]
    }
    {
        /// [rah::find_if_pipeable]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = in | rah::find_if([](int i) { return i == 3; });
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find_if_pipeable]
    }
    {
        /// [rah::find_if_not]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = rah::find_if_not(in, [](int i) { return i < 3; });
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find_if_not]
    }
    {
        /// [rah::find_if_not_pipeable]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = in | rah::find_if_not([](int i) { return i < 3; });
        assert((rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4}))));
        /// [rah::find_if_not_pipeable]
    }

    {
        /// [rah::max_element]
        std::vector<int> in{1, 5, 3, 4};
        auto iter = rah::max_element(in);
        assert(*iter == 5);
        /// [rah::max_element]
    }
    {
        /// [rah::max_element_pred]
        std::vector<std::pair<int, int>> in{{100, 3}, {0, 5}, {0, 1}, {0, 4}};
        auto iter = rah::max_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, 5}));
        /// [rah::max_element_pred]
    }

    {
        /// [rah::min_element]
        std::vector<int> in{1, -5, 3, 4};
        auto iter = rah::min_element(in);
        assert(*iter == -5);
        /// [rah::min_element]
    }

    {
        /// [rah::min_element_pred]
        std::vector<std::pair<int, int>> in{{-100, 3}, {0, -5}, {0, 1}, {0, 4}};
        auto iter = rah::min_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, -5}));
        /// [rah::min_element_pred]
    }

    {
        /// [rah::size]
        std::vector<int> vec3{1, 2, 3};
        assert(rah::size(vec3) == 3);
        /// [rah::size]
    }

    {
        /// [rah::equal]
        std::vector<int> in1{1, 2, 3};
        std::vector<int> in2{1, 2, 3};
        std::vector<int> in3{11, 12, 13};
        assert(rah::equal(in1, in2));
        assert(rah::equal(in1, in3) == false);
        /// [rah::equal]
    }

    /// [rah::empty]
    assert(not(rah::empty(std::vector<int>{1, 2, 3})));
    assert(rah::empty(std::vector<int>()));
    /// [rah::empty]

    {
        /// [rah::copy]
        std::vector<int> in{1, 2, 3};
        std::vector<int> out{0, 0, 0, 4, 5};
        // std::vector<int> out{ 0, 0 }; // Trigger an assert
        assert(rah::equal(
            rah::make_subrange(rah::copy(in, out.begin()), end(out)),
            std::initializer_list<int>({4, 5})));
        assert(out == (std::vector<int>{1, 2, 3, 4, 5}));
        /// [rah::copy]
    }

    {
        /// [rah::copy_if]
        std::vector<int> in{1, 2, 3, 4};
        std::vector<int> out{0, 0, 5, 6};
        assert(rah::equal(
            rah::make_subrange(rah::copy_if(in, out, [](int i) { return i % 2 == 0; }), end(out)),
            std::initializer_list<int>({5, 6})));
        assert(out == (std::vector<int>{2, 4, 5, 6}));
        /// [rah::copy_if]
    }

    {
        /// [rah::fill]
        std::vector<int> out{0, 0, 0, 4, 5};
        rah::fill(out, 42);
        assert(out == (std::vector<int>{42, 42, 42, 42, 42}));
        /// [rah::fill]
    }

    {
        std::vector<int> out{0, 0, 0, 4, 5};
        rah::fill(out | rah::views::take(3), 42);
        assert(out == (std::vector<int>{42, 42, 42, 4, 5}));
    }

    {
        /// [rah::remove_if]
        std::vector<int> in{1, 2, 3, 4, 5};
        auto range_to_erase_begin = rah::remove_if(in, [](auto a) { return a < 4; });
        in.erase(range_to_erase_begin, end(in));
        std::sort(in.begin(), in.end());
        assert(in == std::vector<int>({4, 5}));
        /// [rah::remove_if]
    }
    {
        /// [rah::remove]
        std::vector<int> in{1, 2, 1, 3, 1};
        auto range_to_erase_begin = rah::remove(in, 1);
        in.erase(range_to_erase_begin, end(in));
        std::sort(in.begin(), in.end());
        assert(in == std::vector<int>({2, 3}));
        /// [rah::remove]
    }
    {
        /// [rah::partition]
        std::vector<int> in{1, 2, 3, 4, 5};
        auto boundary = rah::partition(in, [](auto a) { return a >= 4; });
        assert(boundary == in.begin() + 2);
        std::sort(in.begin(), boundary);
        std::sort(boundary, in.end());
        assert(in == std::vector<int>({4, 5, 1, 2, 3}));
        /// [rah::partition]
    }
    {
        /// [rah::stable_partition]
        std::vector<int> in{1, 2, 3, 4, 5};
        auto boundary = rah::stable_partition(in, [](auto a) { return a >= 4; });
        assert(boundary == in.begin() + 2);
        assert(in == std::vector<int>({4, 5, 1, 2, 3}));
        /// [rah::stable_partition]
    }
    {
        /// [rah::sort]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah::sort(in);
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah::sort]
    }
    {
        /// [rah::sort_pred]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah::sort(in, [](auto a, auto b) { return a < b; });
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah::sort_pred]
    }

    /// [rah::stable_sort]
    struct CmpA
    {
        int a;
        int b;
        bool operator<(CmpA rhs) const
        {
            return a < rhs.a;
        }
        bool operator==(CmpA rhs) const
        {
            return a == rhs.a && b == rhs.b;
        }
    };

    {
        std::vector<CmpA> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah::stable_sort(in);
        assert(in == std::vector<CmpA>({{1, 1}, {2, 1}, {2, 2}, {4, 1}, {4, 2}, {4, 3}, {4, 4}}));
    }
    /// [rah::stable_sort]
    {
        /// [rah::stable_sort_pred]
        std::vector<CmpA> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah::stable_sort(in, [](CmpA l, CmpA r) { return l.b < r.b; });
        assert(in == std::vector<CmpA>({{4, 1}, {2, 1}, {1, 1}, {4, 2}, {2, 2}, {4, 3}, {4, 4}}));
        /// [rah::stable_sort_pred]
    }
    {
        /// [rah::shuffle]
        std::random_device rd;
        std::mt19937 g(rd());
        std::vector<int> in{1, 2, 3, 4, 5, 6};
        rah::shuffle(in, g);
        /// [rah::shuffle]
    }
    {
        /// [rah::unique]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah::unique(in), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah::unique]
    }
    {
        /// [rah::unique_pred]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah::unique(in, [](auto a, auto b) { return a == b; }), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah::unique_pred]
    }
    {
        /// [rah::set_difference]
        std::vector<int> in1{1, 3, 4};
        std::vector<int> in2{1, 2, 3};
        std::vector<int> out{0, 0, 0, 0};
        rah::set_difference(in1, in2, out);
        assert(out == std::vector<int>({4, 0, 0, 0}));
        /// [rah::set_difference]
    }
    {
        /// [rah::set_intersection]
        std::vector<int> in1{1, 3, 4};
        std::vector<int> in2{1, 2, 3};
        std::vector<int> out{0, 0, 0, 0};
        rah::set_intersection(in1, in2, out);
        assert(out == std::vector<int>({1, 3, 0, 0}));
        /// [rah::set_intersection]
    }

    // ********************************* test return ref and non-ref ******************************

    using namespace rah;
    using namespace rah::views;
    using namespace std;

    struct Elt
    {
        int member;
        bool operator==(Elt const& elt) const
        {
            return member == elt.member;
        }
        bool operator!=(Elt const& elt) const
        {
            return member != elt.member;
        }
    };

    // Test return reference

    {
        std::vector<Elt> vec = {{0}, {1}, {2}, {3}, {4}};
        auto& r = vec;
        for (auto iter = std::begin(r), end_iter = std::end(r); iter != end_iter; ++iter)
        {
            iter->member = 42; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{42}, {42}, {42}, {42}, {42}})));
        for (auto&& elt : r)
        {
            static_assert(test::is_reference_v<decltype(elt)>, "elt is expected to be a reference");
            elt.member = 78; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{78}, {78}, {78}, {78}, {78}})));
    }
    {
        std::vector<int> vec(5);
        for (int& i : vec | rah::views::transform([](int& i) -> int& { return i; }))
            i = 42; // Check for mutability
        EQUAL_RANGE(vec, (il<int>({42, 42, 42, 42, 42})));
    }

    // Test return non-reference
    {
        std::vector<int> constVect{0, 1, 2, 3};
        EQUAL_RANGE(constVect | transform([](auto a) { return a * 2; }), il<int>({0, 2, 4, 6}));

        std::vector<Elt> vec = {{1}};
        auto r_copy = vec | transform([](auto a) { return Elt{a.member + 1}; });
        for (auto iter = rah::begin(r_copy), end_iter = rah::end(r_copy); iter != end_iter; ++iter)
        {
            assert(iter->member == 2); // Check for mutability
            assert((*iter).member == 2); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(*iter)>
                    || (not test::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_copy)
        {
            assert(elt.member == 2); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(elt)>
                    || (not test::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
        auto r_ref = vec | transform([](auto a) { return a.member; });
        for (auto iter = rah::begin(r_ref), end_iter = rah::end(r_ref); iter != end_iter; ++iter)
        {
            assert(*iter == 1); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(*iter)>
                    || (not test::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_ref)
        {
            assert(elt == 1); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(elt)>
                    || (not test::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
    }

    // **************************** divers compination test ***************************************

    {
        auto genRange = [](size_t i)
        {
            return rah::views::zip(rah::views::repeat(i), rah::views::iota<size_t>(0, 3));
        };
        auto globalRange =
            rah::views::iota<size_t>(0, 4) | rah::views::transform(genRange) | rah::views::join();

        EQUAL_RANGE(
            globalRange,
            (il<std::tuple<size_t, size_t>>{
                {0, 0},
                {0, 1},
                {0, 2},
                {1, 0},
                {1, 1},
                {1, 2},
                {2, 0},
                {2, 1},
                {2, 2},
                {3, 0},
                {3, 1},
                {3, 2}}));
    }

    EQUAL_RANGE(
        (iota(0, 3) | transform([](auto i) { return i * 2; }) | enumerate()),
        (il<std::pair<int64_t, int>>{{0, 0}, {1, 2}, {2, 4}}));

    std::vector<char> vec_abcd{'a', 'b', 'c', 'd'};
    EQUAL_RANGE(
        (vec_abcd | transform([](char i) { return char(i + 1); }) | enumerate()),
        (il<std::pair<int64_t, char>>{{0, 'b'}, {1, 'c'}, {2, 'd'}, {3, 'e'}}));

    // TODO : Make Zip bidirectional when possible
    //EQUAL_RANGE(
    //    (iota(0, 3000, 3) | transform([](auto i) { return i * 2; }) | enumerate() | slice(10, 13)),
    //    (il<std::pair<size_t, int>>{ { 10, 60 }, { 11, 66 }, { 12, 72 } }));

    EQUAL_RANGE(
        (zip(vec_abcd, iota(0, 4))),
        (il<std::tuple<char, int>>{{'a', 0}, {'b', 1}, {'c', 2}, {'d', 3}}));

    EQUAL_RANGE((iota(0, 100) | slice(0, 20) | stride(3)), (il<int>{0, 3, 6, 9, 12, 15, 18}));

    EQUAL_RANGE((iota(10, 15) | reverse()), (il<int>{14, 13, 12, 11, 10}));

    EQUAL_RANGE((iota(0, 100) | slice(10, 15) | reverse()), (il<int>{14, 13, 12, 11, 10}));

    //EQUAL_RANGE(
    //    (iota(10, 15) | enumerate() | reverse()),
    //    (il<std::tuple<size_t, int>>{ { 4, 14 }, { 3, 13 }, { 2, 12 }, { 1, 11 }, { 0, 10 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate() | slice(10, 15)),
    //    (il<std::tuple<size_t, int>>{ { 10, 10 }, { 11, 11 }, { 12, 12 }, { 13, 13 }, { 14, 14 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate() | slice(10, 15) | reverse()),
    //    (il<std::tuple<size_t, int>>{ { 14, 14 }, { 13, 13 }, { 12, 12 }, { 11, 11 }, { 10, 10 } }));

    // iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | rah::to<std::vector<int>>();

    iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | slice(1, 9)
        | rah::to<std::vector<int>>();

    rah::views::zip(
        iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | slice(1, 9), il<int>{2, 4, 6})
        | rah::all_of(PairEqual);

    {
        // Can't compile because generate_n create a forward iterator range
        // int y = 1;
        // EQUAL_RANGE( // test slice in forward iterator
        // 	(rah::views::generate_n(4, [&y]() mutable { auto prev = y; y *= 2; return prev; }) | slice(1, End - 1)),
        // 	(il<int>{ 2, 4 })
        // );
    }

    {
        // Test creation of a custom iterator
        auto gen = customGenerate();
        std::vector<int> gen_copy;
        std::copy(rah::begin(gen), rah::end(gen), std::back_inserter(gen_copy));
        EQUAL_RANGE(gen_copy, std::vector<int>({1, 2, 4, 8}));
    }

    {
        using namespace rah;
        using namespace rah::views;
        int const width = 5;
        int const height = 6;
        int const start = 8;
        int startX = start % width;
        int startY = start / width;
        int const end = 22;
        int endX = end % width;
        int endY = end / width;
        auto getRangeX = [=](int y)
        {
            if (y == startY)
                return std::make_tuple(y, iota(startX, width));
            else if (y == endY)
                return std::make_tuple(y, iota(0, endX));
            else
                return std::make_tuple(y, iota(0, width));
        };

        std::vector<std::atomic<int>> test(width * height);

        auto updateRaw = [&](auto&& y_xRange)
        {
            auto y = std::get<0>(y_xRange);
            auto xRange = std::get<1>(y_xRange);

            for (int x : xRange)
                ++test[x + y * width];
        };

        for (int ySelector : rah::views::iota(0, 3))
        {
            auto rng = rah::views::irange(startY + ySelector, endY + 1, 3) | transform(getRangeX);
            rah::for_each(rng, updateRaw);
        }

        assert(all_of(test | slice(0, start), [](auto&& val) { return val == 0; }));
        assert(all_of(test | slice(start, end), [](auto&& val) { return val == 1; }));
        assert(all_of(test | slice(end, test.size()), [](auto&& val) { return val == 0; }));
    }

    std::cout << "ALL TEST OK" << std::endl;

    return 0;
}
