
x <- display(
    d_h1("this is a h1"),
    d_h2("this is a h2"),
    d_table(iris),
    d_br("this is a br"),
    display(
        d_h2("this is a h3"),
        d_h3("this is a h4"),
        display(
            d_p("this is a p")
        )
    )
)


print(x)





devtools::document() ; devtools::load_all()


x <- diffdf(iris, iris)
disp <- display(
    d_h1("my header"),
    d_br(),
    extract_display(x)
)
print(disp, type = "ascii")





x <- extract_display(diffdf(TDAT, TDAT2))

disp <- display(
    d_h1("my report"),
    x$Attributes,
    d_p("we can now add just a specific value"),
    x$Values$CATEGORICAL
)

print(disp)


