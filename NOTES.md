# NOTES

## consider using corresponding type as Input instead of Unit in components
```
receiver: \a -> Just $ H.action $ left <<< (SetValue a)
```

## consider moving some Enums to  Data.{Formatters.DateTime,Time,Date}

## initial value is not correctly passed to elements (Interval, DateTime)
