#pragma once

#include <rah2/concepts.hpp>
#include <rah2/range_bases.hpp>

#include <iostream>
#include <set>
#include <functional>

#ifdef RAH2_USE_EASTL
#include <EASTL/map.h>
#include <EASTL/vector.h>
#include <EASTL/string.h>
#else
#include <string>
#include <map>
#include <vector>
#endif

#ifndef TEST_LEVEL
#define TEST_LEVEL 1 // medium
#endif

#define STATIC_ASSERT(PRED) static_assert(PRED, #PRED)

struct TestSuite
{
    struct TestResult
    {
        bool success = true;
        size_t testCount = 0;
    };

    RAH2_STD::map<RAH2_STD::string, std::function<void()>> testMap;
    RAH2_STD::map<RAH2_STD::string, TestResult> testResult;
    RAH2_STD::map<RAH2_STD::string, std::set<RAH2_STD::pair<RAH2_STD::string, RAH2_STD::string>>> testCases;

    bool current_test_status = true;
    size_t test_count = 0;
    bool all_success = true;
    volatile size_t avoid_optim = 1;

    void addTest(RAH2_STD::string const& group, RAH2_STD::string const& name, std::function<void()> test)
    {
        testMap.emplace(group + " - " + name, std::move(test));
    }

    char const* currentTest = nullptr;
    void test_case(char const* testcase, char const* extra = "")
    {
#if defined(TEST_DISPLAY_ALL)
        std::cout << "case : " << testcase << "/" << extra << std::endl;
#endif
        testCases[currentTest].emplace(testcase, extra);
    }

    void report() const
    {
        for (auto const& name_test : testMap)
        {
            auto& name = name_test.first;
            std::cout << name_test.first.c_str() << " : ";
            auto iter = testResult.find(name);
            if (iter != testResult.end())
            {
                auto& result = iter->second;
                if (!result.success)
                    std::cout << "FAILED" << std::endl;
                else if (result.testCount == 0)
                    std::cout << "NO TESTS" << std::endl;
                else
                    std::cout << "OK" << std::endl;
            }
            else
                std::cout << "MISSING";

            if (testCases.count(name) != 0u)
            {
                for (auto& caseName_extra : testCases.at(name))
                {
                    std::cout << " / " << caseName_extra.first.c_str();
                }
            }
            std::cout << std::endl;
        }
        std::cout << (all_success ? "Tests passed" : "Tests failed") << std::endl;
    }

    void run()
    {
        for (auto const& name_test : testMap)
        {
            auto& name = name_test.first;
            currentTest = name.c_str();
            auto& test = name_test.second;
            current_test_status = true;
            test_count = 0;
            try
            {
#if defined(TEST_DISPLAY_ALL)
                std::cout << "Testing : " << currentTest << std::endl;
#endif
                test();
            }
            catch (...)
            {
                testResult[name].success = false;
                return;
            }
            testResult[name].success = current_test_status;
            testResult[name].testCount = test_count;
            all_success = all_success && current_test_status;
        }
    }
};

// #define TEST_DISPLAY_ALL
#define TEST_DISPLAY_FAILED
// #define TEST_DISPLAY_NONE

extern TestSuite testSuite;
inline void assert_impl(char const* file, int line, char const* condition, bool value)
{
    ++testSuite.test_count;
#if defined(TEST_DISPLAY_ALL)
    std::cout << file << ":" << line << " assert : " << condition << std::endl;
#endif
    if (value)
    {
#if defined(TEST_DISPLAY_ALL)
        std::cout << "OK" << std::endl;
#endif
    }
    else
    {
#if defined(TEST_DISPLAY_FAILED) and not defined(TEST_DISPLAY_ALL)
        std::cout << file << ":" << line << " assert : " << condition << std::endl;
#endif
#if defined(TEST_DISPLAY_FAILED)
        std::cout << "NOT OK (" << file << ":" << line << ")" << std::endl;
#endif
        // abort();
        testSuite.current_test_status = false;
    }
}

template <typename T>
static std::ostream& operator<<(std::ostream& os, RAH2_STD::vector<T> const& vec)
{
    os << "[";
    for (size_t i = 0; i < vec.size(); ++i)
    {
        os << vec[i];
        if (i != vec.size() - 1)
        {
            os << ", ";
        }
    }
    os << "]";
    return os;
}

namespace RAH2_NS
{
    inline std::ostream& operator<<(std::ostream& os, default_sentinel_t)
    {
        os << "default_sentinel";
        return os;
    }
} // namespace RAH2_NS

template <typename A, typename B>
inline void check_equal_impl(char const* file, int line, bool value, A const& valA, B const& valB)
{
    ++testSuite.test_count;
#if defined(TEST_DISPLAY_ALL)
    std::cout << file << ":" << line << " assert : (" << valA << " == " << valB << ")" << std::endl;
#endif
    if (value)
    {
#if defined(TEST_DISPLAY_ALL)
        std::cout << "OK" << std::endl;
#endif
    }
    else
    {
#if defined(TEST_DISPLAY_FAILED) and not defined(TEST_DISPLAY_ALL)
        std::cout << file << ":" << line << " assert : (" << valA << " == " << valB << ")"
                  << std::endl;
#endif
#if defined(TEST_DISPLAY_FAILED)
        std::cout << "NOT OK (" << file << ":" << line << ") " << std::endl;
#endif
        // abort();
        testSuite.current_test_status = false;
    }
}

#undef assert
#define assert(CONDITION) assert_impl(__FILE__, __LINE__, #CONDITION, (CONDITION))
#define CHECK(CONDITION) assert_impl(__FILE__, __LINE__, #CONDITION, (CONDITION))
#define CHECK_EQUAL(A, B) CHECK((A) == (B))

template <bool A, bool B>
struct AssertEqual;

template <bool X>
struct AssertEqual<X, X>
{
};

template <typename A, typename B>
struct AssertSame;

template <typename X>
struct AssertSame<X, X>
{
};
