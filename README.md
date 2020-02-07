# parsec-test

敗北した  
ここまでが限界  

```shell
ghci> pp (top <* eof) sampleText

[ Block
    [ Mapping ( Key "name" )
        ( Literal ( Scalar "play-ground" ) )
    ]
, Block
    [ Mapping ( Key "version" )
        ( Literal ( Scalar "0.1.0." ) )
    ]
, Block
    [ Mapping ( Key "github" )
        ( Literal ( Scalar "githubuser/play-ground" ) )
    ]
, Block
    [ Mapping ( Key "license" )
        ( Literal ( Scalar "BSD" ) )
    ]
, Block
    [ Mapping ( Key "author" )
        ( Literal ( Scalar "Author name here" ) )
    ]
, Block
    [ Mapping ( Key "maintainer" )
        ( Literal ( Scalar "example@example.com" ) )
    ]
, Block
    [ Mapping ( Key "copyright" )
        ( Literal ( Scalar "2019 Author name here" ) )
    ]
, Block
    [ Mapping ( Key "extra-source-files" )
        ( Block
            [ Sequence
                [ Literal ( Scalar "README.md" )
                , Literal ( Scalar "ChangeLog.md" )
                ]
            ]
        )
    ]
, Block
    [ Mapping ( Key "library" )
        ( Block
            [ Mapping ( Key "source-dirs" )
                ( Literal ( Scalar "src" ) )
            ]
        )
    ]
]
```
