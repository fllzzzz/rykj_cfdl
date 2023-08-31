package com.cf.parking.api.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 防止重复提交
 */

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface PreventRepeat {
    /**
     * 锁方法的名称
     *
     * @return
     */
    String key() default "";

    /**
     * 锁方法的名称
     *
     * @return
     */
    int second() default 1;

    /**
     * 状态(1:小程序,2:PC端)
     *
     * @return
     */
    int state() default 1;
}
