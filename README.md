
# rah2
**rah2** is a **ra**nge, **h**eader-only, C++14/17/20 library. 
It is also compatible with the [EASTL](https://github.com/electronicarts/EASTL).
## What is a range library?
A range is anything that can be iterated. 
In C++, something is a range if we can call `begin(range)` and `end(range)` on it.
Typically, ranges are containers or views.
Views can be generators or adaptors.
Adaptors take a range as input and produce some data in a lazy way.

A range library allows to create ranges and algorithms.
## Why a new range library?
Yes there are some great range libraries in C++, like [range-v3](https://github.com/ericniebler/range-v3) and [boost::range](http://www.boost.org/doc/libs/1_70_0/libs/range).

**rah2** has different goals :
- Port C++20 ranges and constrained algorithms into C++14 and 17
- Port C++20 ranges and constrained algorithms into EASTL (WIP)
- Also, like **EASTL**, **rah2** try to keep good performance in debug mode
## What is inside rah2
- Namespace `rah2` contains all [Constrained algorithms](https://en.cppreference.com/w/cpp/algorithm/ranges)
```cpp
// Usual code with iterators
std::cout << std::count(range.begin(), range.end(), 2);
// Using constrained algorithms
std::cout << rah2::ranges::count(range, 2);
```
- `rah2::views` contains functions to create ranges, actually doing work only when iterated (lazy), saving some memory and simplifying the code, especially when wanting to chain algorithms. [Ranges library](https://en.cppreference.com/w/cpp/ranges)
```cpp
// Usual code with iterators
std::vector<int> ints;
ints.resize(10);
std::iota(range.begin(), range.end(), 0);
std::vector<int> even;
std::copy_if(ints.begin(), ints.end(), std::back_inserter(even), [](int a) {return a % 2 == 0;});
std::vector<int> values;
std::transform(even.begin(), even.end(), std::back_inserter(values), [](int a) {return a * 2;});
for(int i: values)
    std::cout << i << std::endl;
// Using range algorithms
auto values = 
    rah2::views::transform(
        rah2::views::filter(
            rah2::views::iota(0, 10), 
            [](int a) {return a % 2 == 0;})
        [](int a) {return a * 2;});
for(int i: values) // The job in done here, without memory allocation
    std::cout << i << std::endl;
```
- Most of views have a *pipeable* version, making them easier to write and read.
```cpp
// Using pipeable range algorithms
auto values = rah2::views::iota(0, 10)
    | rah2::views::filter([](int a) {return a % 2 == 0;}) 
    | rah2::views::transform([](int a) {return a * 2;});
for(int i: values) // The job in done here, without memory allocation
    std::cout << i << std::endl;
``` 
## License
rah2 is licensed under the [Boost Software License](http://www.boost.org/LICENSE_1_0.txt)
## Documentation
🚧 Documentation is Work In Progress 🚧
## Supported Compilers
- On Windows
    - Visual Studio 2019 && 2022 (/std:c++14, /std:c++17 and /std:c++20)
    - clang++ 13 (-std=c++14, -std=c++17 and -std=c++20)
    - g++ 13 (-std=c++14, -std=c++17 and -std=c++20)
- On Ubuntu
    - clang++ 14 (-std=c++14, -std=c++17 and -std=c++20)
    - g++ 11 (-std=c++14, -std=c++17 and -std=c++20)
- On macos
    - clang++ 14 (-std=c++14, -std=c++17 and -std=c++20)
## Continuous integration
- Using `std` ![CI badge](https://github.com/lhamot/rah2/actions/workflows/cmake.yml/badge.svg)
- Using `eastl` ![CI badge](https://github.com/lhamot/rah2/actions/workflows/cmake_eastl.yml/badge.svg)
## How to use?
- Doc is WIP but cppreference is great
- Views are in **rah2::views** namespace. [Ranges library](https://en.cppreference.com/w/cpp/ranges)
- Algorithms are in **rah2::ranges** namespace [Constrained algorithms](https://en.cppreference.com/w/cpp/algorithm/ranges)
### Simple use
- Add **rah2/includes** in your include path
    - Include **rah2/ranges.hpp** to get the views
    - Include **rah2/algorithm.hpp** to get the algorithms
### How to use with EASTL as backend
- Add the preprocessor definition `RAH2_USE_EASTL`
### How to use as part of the EASTL
- Add the preprocessor definition `RAH2_INSIDE_EASTL`
- Will allow to call rah2 this way ex: `eastl::views::iota(10)`
## The future of **rah2**
- More tests (have to test all iterator category with all algo and all adaptors)
- More optimizations
- Doc
