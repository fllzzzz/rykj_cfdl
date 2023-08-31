package com.cf.parking.api.handler;

import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * @Classname MyMetaObjectHandler
 * @Date 2022/10/20 16:32
 * @Created by csy
 */
@Component
public class MyMetaObjectHandler implements MetaObjectHandler {


    @Override
    public void insertFill(MetaObject metaObject) {
        this.setFieldValByName("createTm",new Date(),metaObject);
        this.setFieldValByName("updateTm",new Date(),metaObject);
    }

    @Override
    public void updateFill(MetaObject metaObject) {
        this.setFieldValByName("updateTm",new Date(),metaObject);
    }
}
