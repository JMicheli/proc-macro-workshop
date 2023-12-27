use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Field, Ident};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  let input_struct_impl = gen_input_struct_impl(&input);
  let builder_struct = gen_builder_struct(&input);
  let builder_struct_impl = gen_builder_struct_impl(&input);

  let output = quote! {
    #input_struct_impl

    #builder_struct
    #builder_struct_impl
  };

  output.into()
}

fn gen_input_struct_impl(input_struct: &DeriveInput) -> TokenStream {
  let input_struct_name = input_struct.ident.clone();
  let builder_struct_name = get_builder_struct_name(&input_struct_name);

  if let syn::Data::Struct(syn::DataStruct {
    fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
    ..
  }) = input_struct.clone().data
  {
    let field_initializers = named.iter().map(|f| gen_field_initializer(f));

    return quote! {
      impl #input_struct_name {
        pub fn builder() -> #builder_struct_name {
          #builder_struct_name {
            #(#field_initializers),*
          }
        }
      }
    };
  }

  panic!("Need to be operating on a structure with named fields!");
}

fn gen_field_initializer(field: &Field) -> TokenStream {
  let field_name = field.ident.clone().unwrap();
  quote! { #field_name: None }
}

fn gen_builder_struct(input_struct: &DeriveInput) -> TokenStream {
  let input_struct_name = input_struct.ident.clone();
  let builder_struct_name = get_builder_struct_name(&input_struct_name);

  if let syn::Data::Struct(syn::DataStruct {
    fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
    ..
  }) = input_struct.clone().data
  {
    let field_declarations = named.iter().map(|f| gen_option_field(f));

    return quote! {
      pub struct #builder_struct_name {
        #(#field_declarations),*
      }
    };
  }

  panic!("Need to be operating on a structure with named fields!");
}

fn gen_builder_struct_impl(input_struct: &DeriveInput) -> TokenStream {
  let input_struct_name = input_struct.ident.clone();
  let builder_struct_name = get_builder_struct_name(&input_struct_name);

  if let syn::Data::Struct(syn::DataStruct {
    fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
    ..
  }) = input_struct.clone().data
  {
    let field_assignors = named.iter().map(|f| gen_field_assignor(f));
    let field_setter_fns = named.iter().map(|f| gen_field_setter_fn(f));

    return quote! {
      impl #builder_struct_name {
        pub fn build(&mut self) -> std::result::Result<#input_struct_name, std::boxed::Box<dyn std::error::Error>> {
          Ok(#input_struct_name {
            #(#field_assignors),*
          })
        }

        #(#field_setter_fns)*
      }
    };
  }

  panic!("Must be using a struct with named fields");
}

fn gen_field_assignor(field: &Field) -> TokenStream {
  let field_name = field.ident.clone().unwrap();
  let field_name_str = field_name.to_string();
  let field_type = field.ty.clone();
  if extract_inner_type(&field_type, "Option").is_some() {
    quote!(#field_name: self.#field_name.clone())
  } else if extract_inner_type(&field_type, "Vec").is_some() {
    quote!(#field_name: self.#field_name.clone().unwrap_or(vec![]))
  } else {
    quote!(#field_name: self.#field_name.clone().ok_or(format!("Field {} was not set", #field_name_str))?)
  }
}

fn gen_field_setter_fn(field: &Field) -> TokenStream {
  let field_name = field.ident.clone().unwrap();
  let mut field_type = field.ty.clone();

  if let Some(inner_ty) = extract_inner_type(&field_type, "Option") {
    // We have Option<T>
    field_type = inner_ty;
  };

  let normal_setter = gen_normal_setter_fn(&field_name, &field_type);

  // Return immediately if there's no attribute1
  if field.attrs.is_empty() {
    normal_setter
  } else {
    match extract_each_attr(&field.attrs) {
      Ok(name) => {
        let each_setter = gen_each_setter_fn(&field_name, &field_type, &name);

        // Check if we have overlapping each and normal names, if so, only return each, otherwise return both
        if name == field_name.to_string() {
          return each_setter;
        } else {
          return quote! {
            #normal_setter
            #each_setter
          };
        }
      }
      Err(e) => e,
    }
  }
}

fn gen_normal_setter_fn(field_name: &Ident, field_type: &syn::Type) -> TokenStream {
  quote! {
    pub fn #field_name(&mut self, value: #field_type) -> &mut Self {
      self.#field_name = Some(value);
      self
    }
  }
}

fn gen_each_setter_fn(field_name: &Ident, field_type: &syn::Type, name: &String) -> TokenStream {
  let method_name = quote::format_ident!("{}", name);
  let inner_type = extract_inner_type(field_type, "Vec").expect("Expected Vec container");

  quote! {
    pub fn #method_name(&mut self, value: #inner_type) -> &mut Self {
      if let std::option::Option::Some(inner_vec) = &mut self.#field_name {
        inner_vec.push(value);
      } else {
        self.#field_name = std::option::Option::Some(vec![value]);
      }

      self
    }
  }
}

fn get_builder_struct_name(ident: &Ident) -> Ident {
  quote::format_ident!("{}Builder", ident)
}

fn gen_option_field(field: &Field) -> TokenStream {
  let ref field_name = field.ident;
  let ref field_type = field.ty;
  if extract_inner_type(&field_type, "Option").is_some() {
    quote!(#field_name: #field_type)
  } else {
    quote!(#field_name: std::option::Option<#field_type>)
  }
}

fn extract_inner_type(ty: &syn::Type, expected_ident: &str) -> Option<syn::Type> {
  if let syn::Type::Path(syn::TypePath {
    path: syn::Path { segments, .. },
    ..
  }) = ty
  {
    if let std::option::Option::Some(syn::PathSegment {
      ident,
      arguments:
        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
    }) = segments.last()
    {
      if ident == expected_ident {
        if let std::option::Option::Some(syn::GenericArgument::Type(ty)) = args.last() {
          return std::option::Option::Some(ty.clone());
        }
      }
    }
  }
  None
}

fn extract_each_attr(attrs: &Vec<syn::Attribute>) -> Result<String, TokenStream> {
  let mut fn_name = String::new();
  if let Some(attr) = attrs.last() {
    match &attr.meta {
      syn::Meta::List(list) => {
        list
          .parse_nested_meta(|meta| {
            if meta.path.is_ident("each") {
              let value = meta.value().unwrap();
              let lit_str: syn::LitStr = value.parse()?; // this parses `"EarlGrey"`
              fn_name = lit_str.value();
              Ok(())
            } else {
              panic!("You gotta have an each!");
            }
          })
          .unwrap();

        return Ok(fn_name);
      }
      _ => {
        panic!("Only lists can be used as attribute inputs (e.g. #[builder(each = \"fn_name\")!")
      }
    }
  }

  unreachable!("Should never extract from empty attr list.")
}
