# Migration to x-sum-type-boilerplate

This document records the migration from the original `sum-type-boilerplate` package to `x-sum-type-boilerplate` for GHC 9.6.7 compatibility.

## 🔄 **Changes Made**

### 1. **Package Replacement**
- **Removed**: `sum-type-boilerplate` (incompatible with GHC 9.6+)
- **Added**: `x-sum-type-boilerplate` (maintained fork with GHC 9.6 support)

### 2. **Files Updated**
- `eventium-core/package.yaml` - Updated dependency
- `examples/bank/package.yaml` - Updated dependency

### 3. **Code Restoration**
- `eventium-core/src/Eventium/TH/SumTypeSerializer.hs` - Restored imports and functions
- `eventium-core/src/Eventium/TH/Projection.hs` - Restored imports and functions

## 🎯 **What This Enables**

With `x-sum-type-boilerplate`, the following Template Haskell functionality is now working:

1. **Sum Type Serializers**: `mkSumTypeSerializer` for converting between sum types
2. **Projection Generation**: `mkProjection` for creating event projections
3. **Boilerplate Generation**: Automatic generation of conversion functions

## 🔧 **Technical Details**

The Template Haskell API changes in GHC 9.6 broke the original `sum-type-boilerplate`:

```haskell
-- GHC ≤ 9.4 (Old API):
ConP name [VarP var]

-- GHC 9.6+ (New API):  
ConP name [] [VarP var]
```

`x-sum-type-boilerplate` includes these fixes and is actively maintained.

## ✅ **Benefits**

- **Full Template Haskell Support**: All TH functions are now working
- **Type Safety**: Automatic generation reduces boilerplate and errors
- **Modern Compatibility**: Works with GHC 9.6.7 and newer
- **Maintained**: Active fork with ongoing support

## 🧪 **Testing**

The Template Haskell code generation can be tested by:

```bash
# Run the build script
./scripts/test-build.sh

# Or manually:
nix develop
cabal build eventium-core examples/bank
```

## 📚 **References**

- [x-sum-type-boilerplate on Hackage](https://hackage.haskell.org/package/x-sum-type-boilerplate)
- [Original sum-type-boilerplate](https://github.com/aleks-sidorenko/sum-type-boilerplate) (archived)
- [GHC 9.6 Template Haskell changes](https://downloads.haskell.org/ghc/9.6.1/docs/users_guide/9.6.1-notes.html) 