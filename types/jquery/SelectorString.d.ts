type ParseSelector <S extends string> = import('typed-query-selector/parser').StrictlyParseSelector<S, Element>


type Whitespace =
  | '\u{9}' // '\t'
  | '\u{A}' // '\n'
  | '\u{B}' // '\v'
  | '\u{C}' // '\f'
  | '\u{D}' // '\r'
  | '\u{20}' // ' '
  | '\u{85}'
  | '\u{A0}'
  | '\u{1680}'
  | '\u{2000}'
  | '\u{2001}'
  | '\u{2002}'
  | '\u{2003}'
  | '\u{2004}'
  | '\u{2005}'
  | '\u{2006}'
  | '\u{2007}'
  | '\u{2008}'
  | '\u{2009}'
  | '\u{200A}'
  | '\u{2028}'
  | '\u{2029}'
  | '\u{202F}'
  | '\u{205F}'
  | '\u{3000}'
  | '\u{FEFF}'
type TrimLeft<V extends string> = V extends `${Whitespace}${infer R}` ? TrimLeft<R> : V
type TrimRight<V extends string> = V extends `${infer R}${Whitespace}` ? TrimRight<R> : V
type Trim<V extends string> = TrimLeft<TrimRight<V>>
type LastArrayElement<ValueType extends readonly unknown[]> = ValueType extends readonly [infer ElementType]
  ? ElementType
  : ValueType extends readonly [infer _, ...infer Tail]
  ? LastArrayElement<Tail>
  : ValueType extends ReadonlyArray<infer ElementType>
  ? ElementType
  : never
type Split<S extends string, Delimiter extends string> = S extends `${infer Head}${Delimiter}${infer Tail}`
  ? [Head, ...Split<Tail, Delimiter>]
  : S extends Delimiter
  ? []
  : [S]
type Taggable = keyof HTMLElementTagNameMap
type Tagged<M extends Taggable> = HTMLElementTagNameMap[M]
type SelectorSeparator = '+' | '~' | '>' | ' '
type SuffixChar = ':' | '.' | '#' | '['
type ExtractSelectors<T extends string> = Trim<Split<T, ','>[number]>
type FinalSelector<T extends string> = Trim<LastArrayElement<Split<T, SelectorSeparator>>>
type GetFinalSelector<T extends string> = FinalSelector<ExtractSelectors<T>>
type BeforeSuffix<T extends string> = Extract<Split<GetFinalSelector<T>, SuffixChar>[0], Taggable>
type InferTag<T extends string> = BeforeSuffix<T> extends Taggable ? HTMLElementTagNameMap[BeforeSuffix<T>] : HTMLElement

type ConditionalKeys<Base, Condition> = NonNullable<
// Wrap in `NonNullable` to strip away the `undefined` type from the produced union.
{
	// Map through all the keys of the given base type.
	[Key in keyof Base]:
	// Pick only keys with types extending the given `Condition` type.
	Base[Key] extends Condition
	// Retain this key since the condition passes.
		? Key
	// Discard this key since the condition fails.
		: never;

	// Convert the produced object into a union type of the keys which passed the conditional test.
}[keyof Base]
>;

type IsEqual<A, B> =
	(<G>() => G extends A ? 1 : 2) extends
	(<G>() => G extends B ? 1 : 2)
		? true
		: false;

type Filter<KeyType, ExcludeType> = IsEqual<KeyType, ExcludeType> extends true ? never : (KeyType extends ExcludeType ? never : KeyType);

type Except<ObjectType, KeysType extends keyof ObjectType> = {
	[KeyType in keyof ObjectType as Filter<KeyType, KeysType>]: ObjectType[KeyType];
};

type ConditionalExcept<Base, Condition> = Except<
  Base,
  ConditionalKeys<Base, Condition>
>

type GettableProperties <E> = ConditionalExcept<E, (...args: any[]) => any>