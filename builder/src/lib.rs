use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  let input_struct_name = input.ident.clone();
  let builder_struct_name = Ident::new(&(input.ident.to_string() + "Builder"), input.ident.span());

  let output = quote! {
    pub struct #builder_struct_name {
      executable: Option<String>,
      args: Option<Vec<String>>,
      env: Option<Vec<String>>,
      current_dir: Option<String>,
    }

    impl #input_struct_name {
      pub fn builder() -> #builder_struct_name {
        #builder_struct_name {
          executable: None,
          args: None,
          env: None,
          current_dir: None,
        }
      }
    }
  };

  output.into()
}
