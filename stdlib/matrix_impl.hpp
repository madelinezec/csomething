#ifndef MATRIX_IMPL_H
#define MATRIX_IMPL_H

#include <iostream>
#include <cstddef>

struct VectorBase {
};

/* this is an "interface" that defines matrix operations
 * This might be a bit over-engineering, but the reasons why we need a base:
 * 1. we need to use dynamic_cast for type checking
 * 2. we can write generic version of C interface functions by calling virtual functions on the base
 */
struct MatrixBase {
    size_t m, n;

    virtual MatrixBase *add(MatrixBase *other) = 0;
    virtual MatrixBase *matrixMultiply(MatrixBase *other) = 0; 
    virtual MatrixBase *applyToVector(VectorBase *vec) = 0;

    bool checkSizeAdd(MatrixBase *) {
        // TODO: implement here
        return false;
    }

    bool checkSizeMultiply(MatrixBase *) {
        // TODO: implement here
        return false;
    }
};

template <typename T>
struct MatrixImpl: public MatrixBase {
    using OwnType = MatrixImpl<T>;
    
    T **data;
    
    template <typename Target>
    Target *type_check(MatrixBase *other) {
        Target *ret = nullptr;
        if (!(ret = dynamic_cast<Target *>(other))) {
            std::cerr << "Type mismatch\n";
            std::exit(-1);
        }
        return ret;
    }

    MatrixBase *add(MatrixBase *other) override {
        OwnType *real_other = type_check<OwnType>(other);
        //TODO: implement here
    }

    MatrixBase *matrixMultiply(MatrixBase *other) override {
        OwnType *real_other = type_check<OwnType>(other); 
        //TODO: implement here
    }

    MatrixBase *matrixMultiply(T scale) {
        // TODO
    }
    //TODO: add other functions
};

#endif
