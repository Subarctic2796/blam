package value

import (
	"fmt"
	"strings"
)

// this is the runtime representation of values for blam
type Value interface {
	String() string
	Type() string
}

// this makes a Value hashable we rely on the go runtime
// to do the heavy lifting of actually figuring out
// how to hash the value
type Hashable interface {
	Value
	Hash()
}

type Null struct{}

func (Null) String() string { return "nil" }
func (Null) Type() string   { return "nil" }
func (Null) Hash()          {}

type Num float64

func (n Num) String() string { return fmt.Sprintf("%g", n) }
func (n Num) Type() string   { return "number" }
func (n Num) Hash()          {}

type Bool bool

func (b Bool) String() string { return fmt.Sprintf("%t", b) }
func (b Bool) Type() string   { return "bool" }
func (b Bool) Hash()          {}

type String string

func (s String) String() string { return string(s) }
func (s String) Type() string   { return "string" }
func (s String) Hash()          {}

// used by closures to store their external state
type ObjUpvalue struct {
	Slot  int
	Value *Value
	Next  *ObjUpvalue
}

func NewObjUpvalue(slot int, val Value) *ObjUpvalue {
	return &ObjUpvalue{slot, &val, nil}
}

func (o *ObjUpvalue) String() string { return fmt.Sprintf("<upvalue %s>", *o.Value) }
func (o *ObjUpvalue) Type() string   { return "upvalue" }

type ObjFn struct {
	Arity, UpvalueCnt int
	Chunk             *Chunk
	Name              String
}

func NewObjFn() *ObjFn { return &ObjFn{0, 0, NewChunk(), ""} }

func (f *ObjFn) String() string {
	if f.Name == "" {
		return "<script>"
	}
	return fmt.Sprintf("<fn %s>", f.Name)
}

func (f *ObjFn) Type() string { return "function" }

type ObjClos struct {
	*ObjFn
	Upvalues []*ObjUpvalue
}

func NewObjClos(fn *ObjFn) *ObjClos {
	return &ObjClos{fn, make([]*ObjUpvalue, fn.UpvalueCnt)}
}

func (c *ObjClos) String() string { return c.ObjFn.String() }
func (c *ObjClos) TypeOf() string { return "closure" }

type NativeFn func(argc int, args ...Value) Value

type ObjNativeFn struct {
	Name String
	Fn   NativeFn
}

func (f *ObjNativeFn) String() string { return fmt.Sprintf("<native fn %s>", f.Name) }
func (f *ObjNativeFn) Type() string   { return "native function" }

type ObjArray struct {
	Elements []Value
}

func NewObjArray(elements []Value) *ObjArray { return &ObjArray{elements} }

func (a *ObjArray) String() string {
	elements := make([]string, 0, len(a.Elements))
	for _, e := range a.Elements {
		elements = append(elements, e.String())
	}
	return fmt.Sprintf("[%s]", strings.Join(elements, ", "))
}

func (a *ObjArray) Type() string { return "array" }

type ObjMap struct {
	Pairs map[Value]Value
}

func NewObjMap(pairs map[Value]Value) *ObjMap { return &ObjMap{pairs} }

func (m *ObjMap) String() string {
	pairs := make([]string, 0, len(m.Pairs))
	for k, v := range m.Pairs {
		pairs = append(pairs, fmt.Sprintf("%s: %s", k, v))
	}
	return fmt.Sprintf("{%s}", strings.Join(pairs, ", "))
}

func (m *ObjMap) Type() string { return "map" }

type ObjError struct {
	Message String
}

func (e *ObjError) String() string { return e.Message.String() }
func (e *ObjError) Type() string   { return "error" }
func (e *ObjError) Error() string  { return e.Message.String() }
func (e *ObjError) Hash()          {}

type ObjClass struct {
	Name    String
	Init    Value   // either *ObjClos / *ObjNativeFn
	Methods []Value // methods are dispacted by index
}

func NewObjClass(name String) *ObjClass {
	return &ObjClass{name, nil, make([]Value, 0)}
}

func (c *ObjClass) String() string { return c.Name.String() }
func (c *ObjClass) Type() string   { return fmt.Sprintf("class('%s')", c.Name) }

type ObjInstance struct {
	Klass  *ObjClass
	Fields []Value // fields are accessed by index
}

func NewObjInstance(klass *ObjClass) *ObjInstance {
	return &ObjInstance{klass, make([]Value, 0)}
}

func (i *ObjInstance) String() string {
	fields := make([]string, 0, len(i.Fields))
	for _, v := range i.Fields {
		fields = append(fields, v.String())
	}
	return fmt.Sprintf("%s{%s}", i.Klass.Name, strings.Join(fields, " "))
}

func (i *ObjInstance) Type() string { return fmt.Sprintf("instance('%s')", i.Klass.Name) }
func (i *ObjInstance) Hash()        {}

type ObjBoundMethod struct {
	// really *ObjInstance
	Receiver Value
	// can change this to Value so that we can have *ObjClos or *ObjNativeFn
	Method *ObjClos
}

func NewObjBoundMethod(receiver Value, method *ObjClos) *ObjBoundMethod {
	return &ObjBoundMethod{receiver, method}
}

func (o *ObjBoundMethod) String() string { return o.Method.String() }
func (o *ObjBoundMethod) TypeOf() string {
	return fmt.Sprintf("%s method", o.Receiver.(*ObjInstance).Klass.Name)
}
