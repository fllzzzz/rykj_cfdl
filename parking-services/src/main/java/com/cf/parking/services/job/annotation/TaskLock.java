package com.cf.parking.services.job.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * task
 */

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface TaskLock {
    /**
     * 锁方法的名称
     *
     * @return
     */
    String key() default "";


}
