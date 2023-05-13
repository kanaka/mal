#ifndef BIG_COMPLEX_H
#define BIG_COMPLEX_H

/* The following code applies the GNU GMP library, which is
   licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <string>
#include <gmpxx.h>


class BigComplex
{
public:
    BigComplex(): real_part(0), imaginary_part(0) {};
    BigComplex(std::string repr);
    mpq_class real() {return real_part;};
    mpq_class imag() {return imag_part;};
    BigComplex operator=(BigComplex& const c);
    BigComplex operator+(const BigComplex& const a, BigComplex& const b);
    BigComplex operator-(const BigComplex& const a, BigComplex& const b);
    BigComplex operator*(const BigComplex& const a, BigComplex& const b);
    BigComplex operator/(const BigComplex& const a, BigComplex& const b);
    BigComplex operator+=(BigComplex& const c);
    BigComplex operator-=(BigComplex& const c);
    BigComplex operator*=(BigComplex& const c);
    BigComplex operator/=(BigComplex& const c);

private:
    mpq_class real_part, imaginary_part;
}


#endif