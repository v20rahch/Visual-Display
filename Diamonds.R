dd <- diamonds

i <- ggplot(data=dd,aes(x=carat,y=price, 
                             color=clarity))
i <- ggplot(data=dd,aes(x=carat,y=price, 
                        color=cut))

i <- ggplot(data=dd,aes(x=carat,y=price, 
                        color=depth))

i + geom_point() + geom_smooth(fill=NA)
summary(dd)

