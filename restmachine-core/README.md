# Restmachine

Restmachine is a web framework for creating REST-ful services loosely based on Webmachine.

## Resources

A `Resource` is a type where all functions are defined.

    data Resource = Resource 
      { _exists :: Bool
      , _serviceAvailable :: Bool
      , _isAuthorize :: Bool
      , _forbidden :: Bool
      , ...
      }

As the lenses are created for `Resource` you can access the fields using lenses:

    defaultResource & exists .= True
                    & serviceAvailable .= True
                    ..


