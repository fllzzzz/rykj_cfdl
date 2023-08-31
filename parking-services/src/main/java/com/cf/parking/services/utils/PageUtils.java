package com.cf.parking.services.utils;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.support.result.PageRequest;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 用于mybaties-plus 的请求和返回的分页参数转换
 *
 * @Classname PageUtils
 * @Date 2022/10/24 10:44
 * @Created by csy
 */
public class PageUtils {
    /**
     * 将前端的请求参数转换成 mybaties-plus 的分页请求参数
     * @param param 分页请求参数
     * @return page
     */
    public static <T> Page<T> toPage(PageRequest param) {
        return new Page<T>().setCurrent(param.getPageNo()).setSize(param.getPageSize());
    }

    /**
     *  将mybaties-plus的返回参数转换成指定类型的接口返回参数
     * @param page 分页
     * @param targetClass 指定类型
     * @return 返回
     * @param <T> 泛型
     */
    public static <T,S> PageResponse<T> toResponse(IPage<S> page, Class<T> targetClass) {
        List<T> resultList = BeanConvertorUtils.copyList(page.getRecords(), targetClass);
        return new PageResponse<>(resultList, page.getCurrent(), page.getTotal(), page.getSize());
    }

    /**
     * 将mybaties-plus的转换参数和列表转换成接口返回分页参数
     * @param page 分页
     * @param resultList 已经转换好的参数
     * @return 接口返回分页参数
     * @param <T> 转换后
     * @param <S> 转换前
     */
    public static <T, S> PageResponse<T> toResponseList(IPage<S> page, List<T> resultList) {
        return new PageResponse<>(resultList, page.getCurrent(), page.getTotal(), page.getSize());
    }

    /**
     * 接口返回result
     *
     * @param result     分页
     * @param resultList 返回
     * @param <T>        转换后
     * @param <S>        转换前
     * @return 接口返回分页参数
     */
    public static <T, S> Result pageResult(PageResponse<S> result, List<T> resultList) {
        return Result.buildSuccessResult(new PageResponse(resultList, result.getPageNo(), result.getTotal(), result.getPageSize()));
    }

    /**
     * 将mybaties-plus的转换参数转换成接口返回分页参数
     *
     * @param page 分页
     * @param <T>  泛型
     * @return
     */
    public static <T> PageResponse<T> emptyResponseList(IPage<T> page) {
        return new PageResponse<>(new ArrayList<>(), page.getCurrent(), page.getTotal(), page.getSize());
    }

    /**
     * 将mybaties-plus的转换参数转换成接口返回分页参数
     *
     * @param page 分页
     * @param <T>  泛型
     * @return
     */
    public static <T> PageResponse emptyResponseList(Page<T> page) {
        return new PageResponse<>(new ArrayList<>(), page.getCurrent(), 0L, page.getSize());
    }

    /**
     * 接口判空返回result
     *
     * @param result 返回
     * @param <T>    泛型
     * @return
     */
    public static <T> Result emptyPageResult(PageResponse<T> result) {
        return Result.buildSuccessResult(new PageResponse(new ArrayList(), result.getPageNo(), result.getTotal(), result.getPageSize()));
    }
}
